package slyce.core

import java.io.File

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.infixOrderingOps

import klib.Implicits._
import klib.fp.types._
import klib.utils._

final case class Source(input: String, name: Maybe[String] = None) {
  val chars: List[Char] = input.toList

  def mark(
      messages: List[Marked[String]],
      config: Source.Config = Source.Config.Default,
  ): String = {
    // TODO (KR) : Possibly do something separate with EOF/Unknown (?)
    val (eofs, _marked) =
      messages.partitionMap {
        case Marked(msg, span) =>
          span match {
            case highlight: Span.Highlight => scala.Right((msg, highlight))
            case _                         => scala.Left(msg)
          }
      }
    val marked =
      _marked
        .sortBy(_._2.start)
        .groupMap(_._2)(_._1)
        .toList
        .sortBy(_._1.start)

    val (goodMarked, badMarked) = {
      @tailrec
      def filter(
          min: Span.Pos,
          unseen: List[(Span.Highlight, List[String])],
          good: List[(Span.Highlight, List[String])],
          bad: List[List[String]],
      ): (List[(Span.Highlight, List[String])], List[List[String]]) =
        unseen match {
          case (head @ (span, msgs)) :: tail =>
            if (span.start <= min)
              filter(
                min,
                tail,
                good,
                msgs.map(m => s"[$span]: $m") :: bad,
              )
            else
              filter(
                span.end,
                tail,
                head :: good,
                bad,
              )
          case Nil =>
            (good.reverse, bad.reverse)
        }

      filter(
        Span.Pos(0, 0, 1),
        marked,
        Nil,
        Nil,
      )
    }

    def getColorAndNext(colors: NonEmptyList[ColorString.Color]): (ColorString.Color, NonEmptyList[ColorString.Color]) =
      (
        colors.head,
        colors.tail.toNel.getOrElse(config.colors),
      )

    def diffColor(color: ColorString.Color): Maybe[(String, String)] =
      color.toColorState.colorizeAndDeColorize(ColorString.ColorState.Default)

    // ---  ---

    val maxLineNoStrLength =
      goodMarked
        .map(_._1.start.lineNo.toString)
        .maxOption
        .toMaybe
        .cata(_.length, 0)

    def lineNoLabel(lineNo: Int): String = {
      val lineNoStr = lineNo.toString
      s"${" " * (maxLineNoStrLength - lineNoStr.length)}$lineNoStr : "
    }

    val stringBuilder = new StringBuilder

    final case class Show(
        span: Span.Highlight,
        messages: List[String],
        colorizeAndDeColorize: Maybe[(String, String)],
    )

    sealed trait State
    object State {
      final case class Unsure(first: Boolean) extends State
      final case class SkippingLine(first: Boolean) extends State
      final case class ShowingLine(
          doneForLine: List[Show],
      ) extends State
      final case class Displaying(
          current: Show,
          doneForLine: List[Show],
      ) extends State
    }

    def writeMessages(
        messages: List[String],
        colorizeAndDeColorize: Maybe[(String, String)],
        markerString: String,
        indentString: String,
        isFirst: Boolean,
    ): Unit = {
      val adjustedIdtString =
        colorizeAndDeColorize match {
          case Some((colorize, deColorize)) =>
            s"\n$colorize$indentString$deColorize"
          case None =>
            s"\n$indentString"
        }
      val msgs = messages.map(_.replaceAll("\n", adjustedIdtString))
      msgs match {
        case mHead :: mTail =>
          if (!isFirst)
            stringBuilder.append('\n')
          colorizeAndDeColorize match {
            case Some((colorize, deColorize)) =>
              stringBuilder
                .append(colorize)
                .append(markerString)
                .append(deColorize)
                .append(mHead)
              mTail.foreach { msg =>
                stringBuilder
                  .append('\n')
                  .append(colorize)
                  .append(markerString)
                  .append(deColorize)
                  .append(msg)
              }
            case None =>
              stringBuilder
                .append(markerString)
                .append(mHead)
              mTail.foreach { msg =>
                stringBuilder
                  .append('\n')
                  .append(markerString)
                  .append(msg)
              }
          }
        case Nil =>
      }
    }
    def writeMessageList(
        messages: List[(List[String], Maybe[(String, String)])],
        markerString: String,
        indentString: String,
    ): Unit = {
      @tailrec
      def loop(messages: List[(List[String], Maybe[(String, String)])], first: Boolean): Unit =
        messages match {
          case (msgs, cdc) :: tail =>
            writeMessages(msgs, cdc, markerString, indentString, first)
            loop(tail, false)
          case Nil =>
        }

      loop(messages, true)
    }

    // NOTE : By the time this is called, `waiting` is assumed to be in a good state,
    //      : where it does not need to be checked anymore
    @tailrec
    def printSpanMessages(
        pos: Span.Pos,
        state: State,
        chars: List[Char],
        waiting: List[(Span.Highlight, List[String])],
        colors: NonEmptyList[ColorString.Color],
    ): NonEmptyList[ColorString.Color] =
      chars match {
        case cHead :: cTail =>
          val nextPos = pos.onChar(cHead)
          cHead match {
            case '\n' =>
              state match {
                case unsure @ State.Unsure(_) =>
                  printSpanMessages(
                    nextPos,
                    unsure,
                    cTail,
                    waiting,
                    colors,
                  )
                case State.SkippingLine(first) =>
                  printSpanMessages(
                    nextPos,
                    State.Unsure(first),
                    cTail,
                    waiting,
                    colors,
                  )
                case State.ShowingLine(doneForLine) =>
                  // TODO (KR) : Might need to start something here
                  stringBuilder.append('\n')
                  writeMessageList(
                    doneForLine.reverseMap(dfl => (dfl.messages, dfl.colorizeAndDeColorize)),
                    config.markerString,
                    config.markerIndentString,
                  )
                  printSpanMessages(
                    nextPos,
                    State.Unsure(false),
                    cTail,
                    waiting,
                    colors,
                  )
                case State.Displaying(current, doneForLine) =>
                  // TODO (KR) : Might need to stop something here
                  stringBuilder.append('\n')
                  current.colorizeAndDeColorize.foreach(cdc => stringBuilder.append(cdc._2))
                  writeMessageList(
                    doneForLine.reverseMap(dfl => (dfl.messages, dfl.colorizeAndDeColorize)),
                    config.markerString,
                    config.markerIndentString,
                  )
                  stringBuilder
                    .append('\n')
                    .append(lineNoLabel(nextPos.lineNo))
                  current.colorizeAndDeColorize.foreach(cdc => stringBuilder.append(cdc._1))
                  printSpanMessages(
                    nextPos,
                    State.Unsure(false),
                    cTail,
                    waiting,
                    colors,
                  )
              }
            case cHead =>
              state match {
                case State.Unsure(first) =>
                  waiting match {
                    case wHead :: _ if wHead._1.start.lineNo == pos.lineNo =>
                      if (!first)
                        stringBuilder.append('\n')
                      stringBuilder.append(lineNoLabel(wHead._1.start.lineNo))
                      printSpanMessages(
                        pos,
                        State.ShowingLine(Nil),
                        chars,
                        waiting,
                        colors,
                      )
                    case _ =>
                      printSpanMessages(
                        nextPos,
                        State.SkippingLine(first),
                        cTail,
                        waiting,
                        colors,
                      )
                  }
                case skip @ State.SkippingLine(_) =>
                  printSpanMessages(
                    nextPos,
                    skip,
                    cTail,
                    waiting,
                    colors,
                  )
                case State.ShowingLine(doneForLine) =>
                  waiting match {
                    case wHead :: wTail if wHead._1.start == pos =>
                      val (color, nextColors) = getColorAndNext(colors)
                      val show = Show(wHead._1, wHead._2, diffColor(color))

                      show.colorizeAndDeColorize.foreach(cdc => stringBuilder.append(cdc._1))
                      printSpanMessages(
                        pos,
                        State.Displaying(show, doneForLine),
                        chars,
                        wTail,
                        nextColors,
                      )
                    case _ =>
                      stringBuilder.append(cHead)
                      printSpanMessages(
                        nextPos,
                        state,
                        cTail,
                        waiting,
                        colors,
                      )
                  }
                case State.Displaying(current, doneForLine) =>
                  stringBuilder.append(cHead)
                  if (current.span.end == pos) {
                    current.colorizeAndDeColorize.foreach(cdc => stringBuilder.append(cdc._2))
                    printSpanMessages(
                      nextPos,
                      State.ShowingLine(current :: doneForLine),
                      cTail,
                      waiting,
                      colors,
                    )
                  } else {
                    printSpanMessages(
                      nextPos,
                      state,
                      cTail,
                      waiting,
                      colors,
                    )
                  }
              }
          }
        case Nil =>
          state match {
            case State.Unsure(_) =>
              colors
            case State.SkippingLine(_) =>
              colors
            case State.ShowingLine(doneForLine) =>
              stringBuilder.append('\n')
              writeMessageList(
                doneForLine.reverseMap(dfl => (dfl.messages, dfl.colorizeAndDeColorize)),
                config.markerString,
                config.markerIndentString,
              )
              colors
            case State.Displaying(current, doneForLine) =>
              ??? // TODO (KR) : Should be an error? Just close it off?
          }
      }

    def printEofMessages(
        waiting: List[List[String]],
        colors: NonEmptyList[ColorString.Color],
    ): Unit = {
      @tailrec
      def loop(
          waiting: List[List[String]],
          colors: NonEmptyList[ColorString.Color],
          stack: List[(List[String], Maybe[(String, String)])],
      ): List[(List[String], Maybe[(String, String)])] =
        waiting match {
          case head :: tail =>
            val (color, nextColors) = getColorAndNext(colors)
            loop(
              tail,
              nextColors,
              (head, diffColor(color)) :: stack,
            )
          case Nil =>
            stack.reverse
        }

      writeMessageList(
        loop(waiting, colors, Nil),
        config.eofMarkerString,
        config.eofMarkerIndentString,
      )
    }

    // ---  ---

    if (config.showName)
      name match {
        case Some(name) => stringBuilder.append(s"source: $name\n")
        case None       => stringBuilder.append("source: [UNKNOWN]\n")
      }

    val colorsAfterSpanMessages =
      printSpanMessages(
        Span.Pos.Start,
        State.Unsure(true),
        chars,
        goodMarked,
        config.colors,
      )

    val allEof = eofs.map(_ :: Nil) ::: badMarked

    if (goodMarked.nonEmpty && allEof.nonEmpty)
      stringBuilder.append('\n')

    printEofMessages(
      allEof,
      colorsAfterSpanMessages,
    )

    stringBuilder.toString
  }

}
object Source {

  def fromFile(file: File): IO[Source] =
    IO.readFile(file)
      .map(Source(_, file.toString.some))

  def markAll(
      msgs: List[Marked[String]],
      config: Config = Config.Default,
  ): String = {
    val (unknownSource, knownSource) =
      msgs
        .groupBy(_.span.mSource)
        .toList
        .partitionMap {
          case (mSource, msgs) =>
            mSource match {
              case Some(source) => scala.Right((source, msgs))
              case None         => scala.Left(msgs)
            }
        }

    val fromKnown =
      knownSource.map {
        case (source, msgs) =>
          source.mark(msgs, config)
      }

    val fromUnknown =
      unknownSource.flatten.ensure(_.nonEmpty).map(Source("").mark(_, config))

    (fromKnown ++ fromUnknown.toOption).mkString("\n\n")
  }

  final case class Config(
      showName: Boolean,
      markerString: String,
      markerIndentString: String,
      eofMarkerString: String,
      eofMarkerIndentString: String,
      colors: NonEmptyList[ColorString.Color],
  )
  object Config {

    val Default: Config =
      Config(
        showName = true,
        // format: off
        markerString =          "    *** ",
        markerIndentString =    "     >  ",
        eofMarkerString =       "      * ",
        eofMarkerIndentString = "     >  ",
        // format: on
        colors = NonEmptyList
          .nel(
            Color.Named.Red,
            Color.Named.Green,
            Color.Named.Yellow,
            Color.Named.Blue,
            Color.Named.Magenta,
            Color.Named.Cyan,
          )
          .map(c => ColorString.Color(c.some, None)),
      )

  }

}
