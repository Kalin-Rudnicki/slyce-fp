package slyce.generate.main

import java.io.File

import org.rogach.scallop._

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils._
import klib.utils._
import klib.utils.Logger.{helpers => L}
import klib.utils.Logger.helpers.Implicits._

import slyce.core._
import slyce.core.Marked.Implicits._
import slyce.generate._
import slyce.generate.input._
import slyce.generate.parsers.{lexer => Lex, grammar => Gram}
import slyce.parse.Attempt

object Main {
  val LexerExtension = "slf"
  val GrammarExtension = "sgf"

  final case class Pair(
      pkg: List[String],
      baseName: String,
      lexerFile: File,
      grammarFile: File,
  ) {
    val scopedName: String = s"${pkg.mkString(".")}.$baseName"

    def generate(
        logger: Logger,
        outputDir: File,
        debugOutputDir: Maybe[File],
    ): IO[Unit] = {
      def convertLexer(lexerSource: Source): Attempt[Lexer] = {
        def convertMode(mode: Lex.NonTerminal.Mode): Lexer.Mode = {
          def convertLine(line: Lex.NonTerminal.Line): Lexer.Mode.Line = {
            def convertChar(char: Lex.Tok.char): Char =
              char.text.head
            def convertEscChar(escChar: Lex.Tok.escChar): Char =
              escChar.text match {
                case "n" => '\n'
                case "t" => '\t'
                case t   => t.head
              }

            def convertRegex(reg: Lex.NonTerminal.Regex): Regex =
              reg match {
                case Lex.NonTerminal.Regex._1(group) =>
                  convertGroupInner(group.lift)
                case Lex.NonTerminal.Regex._2(charClass) =>
                  convertCharClass(charClass)
                case Lex.NonTerminal.Regex._3(regex, quant) =>
                  val convertedReg: Regex = convertRegex(regex)
                  val (min, max) =
                    quant match {
                      case Lex.NonTerminal.Quant._1(_) =>
                        (0, 1.some)
                      case Lex.NonTerminal.Quant._2(_) =>
                        (0, None)
                      case Lex.NonTerminal.Quant._3(_) =>
                        (1, None)
                      case Lex.NonTerminal.Quant._4(_, num, _) =>
                        val i = num.toMaybe.map(_.text.toInt)
                        (i.getOrElse(0), i)
                      case Lex.NonTerminal.Quant._5(_, min, _, max, _) =>
                        (
                          min.toMaybe.cata(_.text.toInt, 0),
                          max.toMaybe.map(_.text.toInt),
                        )
                    }
                  convertedReg.repeat(min, max)
              }
            def convertGroupInner(gi: Lex.NonTerminal.GroupInner): Regex =
              Regex.Group(gi.toNonEmptyList.map(convertSequence))
            def convertSequence(seq: Lex.NonTerminal.Sequence): Regex.Sequence =
              Regex.Sequence(seq.toList.map(convertRegex))
            def convertCharClass(cc: Lex.NonTerminal.CharClass): Regex = {
              def convertEscChars(escChars: Lex.Tok.escChars): InfiniteSet[Char] =
                escChars.text match {
                  case "d" => InfiniteSet.Inclusive('0'.to('9').toSet)
                  case "." => InfiniteSet.Exclusive()
                  case _   => ???
                }

              cc match {
                case Lex.NonTerminal.CharClass._1(_, inverse, ccChars, _) =>
                  val isInverse = inverse.toMaybe.nonEmpty

                  def convertCCChars(ccChars: Lex.NonTerminal.CCChars): Regex.CharClass = {
                    def convertCCChar(ccChar: Lex.NonTerminal.CCChar): Char =
                      ccChar match {
                        case Lex.NonTerminal.CCChar._1(char)    => convertChar(char)
                        case Lex.NonTerminal.CCChar._2(escChar) => convertEscChar(escChar)
                      }

                    ccChars match {
                      case Lex.NonTerminal.CCChars._1(rLeft, _, rRight) =>
                        val c1 = convertCCChar(rLeft)
                        val c2 = convertCCChar(rRight)
                        val (rangeLeft, rangeRight) = (c1 <= c2) ? (c1, c2) | (c2, c1)
                        isInverse ?
                          Regex.CharClass.exclusiveRange(rangeLeft, rangeRight) |
                          Regex.CharClass.inclusiveRange(rangeLeft, rangeRight)
                      case Lex.NonTerminal.CCChars._2(ccChar) =>
                        isInverse ?
                          Regex.CharClass.exclusive(convertCCChar(ccChar)) |
                          Regex.CharClass.inclusive(convertCCChar(ccChar))
                      case Lex.NonTerminal.CCChars._3(escChars) =>
                        val cc = Regex.CharClass(convertEscChars(escChars))
                        isInverse ? cc.~ | cc
                    }
                  }

                  Regex.CharClass.union(ccChars.toNonEmptyList.map(convertCCChars).toList: _*)
                case Lex.NonTerminal.CharClass._2(char) =>
                  Regex.CharClass.inclusive(convertChar(char))
                case Lex.NonTerminal.CharClass._3(escChar) =>
                  Regex.CharClass.inclusive(convertEscChar(escChar))
                case Lex.NonTerminal.CharClass._4(escChars) =>
                  Regex.CharClass(convertEscChars(escChars))
              }
            }

            def convertYield(y: Lex.NonTerminal.Yield): Marked[Yields.Yield] = {
              val Lex.NonTerminal.Yield(yieldType, subString) = y
              val bounds =
                subString match {
                  case Lex.NonTerminal.SubString._1(_, oInt, _) =>
                    val i = oInt.toMaybe.map(_.text.toInt)
                    (i, i)
                  case Lex.NonTerminal.SubString._2(_, oInt1, _, oInt2, _) =>
                    (oInt1.toMaybe.map(_.text.toInt), oInt2.toMaybe.map(_.text.toInt))
                }
              yieldType match {
                case Lex.NonTerminal.YieldType._1(at) =>
                  Yields.Yield.Text(bounds).marked(at.span)
                case Lex.NonTerminal.YieldType._2(term) =>
                  Yields.Yield.Terminal(term.text).marked(term.span)
                case Lex.NonTerminal.YieldType._3(raw) =>
                  Yields.Yield
                    .Const(
                      raw.lift.toNonEmptyList.map {
                        _.lift match {
                          case chars: Lex.Tok.chars     => chars.text
                          case escChar: Lex.Tok.escChar => convertEscChar(escChar).toString
                        }
                      }.mkString,
                      bounds,
                    )
                    .marked // TODO (KR) : Use actual bounds
              }
            }

            def convertToMode(tm: Lex.NonTerminal.OptToMode): Marked[Yields.ToMode[String]] =
              tm.toMaybe match {
                case Some(tm) =>
                  tm match {
                    case Lex.NonTerminal.ToMode._1(to, mode)   => Yields.ToMode.To(mode.text).marked(Span.joinNE(to.span, mode.span))
                    case Lex.NonTerminal.ToMode._2(push, mode) => Yields.ToMode.Push(mode.text).marked(Span.joinNE(push.span, mode.span))
                    case Lex.NonTerminal.ToMode._3(pop)        => Yields.ToMode.Pop.marked(pop.span)
                  }
                case None =>
                  Yields.ToMode.Same.marked
              }

            Lexer.Mode.Line(
              priority = line._1.span.start.lineNo,
              regex = convertGroupInner(line._0).marked, // TODO (KR) : Proper marking
              yields = Yields(
                yields = line._2.toList.map(convertYield),
                toMode = convertToMode(line._3),
              ),
            )
          }

          Lexer.Mode(
            name = mode._1.markedText,
            lines = mode._2.toNonEmptyList.map(convertLine).toList, // TODO (KR) : Should be NonEmptyList
          )
        }

        for {
          ntRoot <- Lex.parser.parse(lexerSource)
        } yield Lexer(
          startMode = ntRoot._1.markedText,
          modes = ntRoot._2.toNonEmptyList.map(convertMode).toList, // TODO (KR) : Should be NonEmptyList
        )
      }

      def convertGrammar(grammarSource: Source): Attempt[Grammar] = {
        def convertNt(nt: Gram.NonTerminal.NT): Grammar.NT = {
          def convertElement(element: Gram.NonTerminal.Element): Marked[Grammar.Element] = {
            def convertNonOptElement(nonOptElement: Gram.NonTerminal.NonOptElement): Marked[Grammar.NonOptElement] =
              nonOptElement.lift match {
                case nt: Gram.Tok.nonTerminal =>
                  Grammar.Identifier.unsafeNonTerminal(nt.markedText)
                case t: Gram.Tok.terminal =>
                  Grammar.Identifier.unsafeTerminal(t.markedText)
                case list: Gram.NonTerminal.AnonList =>
                  list match {
                    case Gram.NonTerminal.AnonList._1(unIgnored, listType) =>
                      convertListType(listType).simple(convertElement(unIgnored)).marked // TODO (KR) : Bounds
                    case Gram.NonTerminal.AnonList._2(_, start, _, listType) =>
                      Grammar
                        .ListNonTerminal(
                          `type` = convertListType(listType),
                          start = convertIgnoredList(start),
                          repeat = None,
                        )
                        .marked // TODO (KR) : Bounds
                    case Gram.NonTerminal.AnonList._3(_, start, _, repeat, _, listType) =>
                      Grammar
                        .ListNonTerminal(
                          `type` = convertListType(listType),
                          start = convertIgnoredList(start),
                          repeat = convertIgnoredList(repeat).some,
                        )
                        .marked // TODO (KR) : Bounds
                  }
                case Gram.NonTerminal.Raw(_, raw, _) =>
                  Grammar.Identifier
                    .raw(
                      raw.toNonEmptyList.map {
                        _.lift match {
                          case chars: Gram.Tok.chars =>
                            chars.text
                          case escChar: Gram.Tok.escChar =>
                            escChar.text match {
                              case "n" => "\n"
                              case "t" => "\t"
                              case t   => t
                            }
                        }
                      }.mkString,
                    )
                    .marked // TODO (KR) : proper bounds
              }

            val nonOpt = convertNonOptElement(element._0)
            element._1.toMaybe.nonEmpty ? nonOpt.map(Grammar.Optional) | nonOpt
          }

          def convertListType(listType: Gram.Tok.listType): Grammar.ListNonTerminal.Type =
            listType.text match {
              case "*" => Grammar.ListNonTerminal.Type.*
              case "+" => Grammar.ListNonTerminal.Type.+
              case _   => ???
            }

          def convertIgnoredList(uiList: Gram.NonTerminal.UnIgnoredElementList): IgnoredList[Marked[Grammar.Element]] =
            uiList match {
              case Gram.NonTerminal.UnIgnoredElementList._1(element) =>
                IgnoredList(
                  before = Nil,
                  unIgnored = convertElement(element),
                  after = Nil,
                )
              case Gram.NonTerminal.UnIgnoredElementList._2(before, _, unIgnored, after) =>
                IgnoredList(
                  before = before.toList.map(convertElement),
                  unIgnored = convertElement(unIgnored),
                  after = after.toList.map(convertElement),
                )
            }

          def convertStandardNT(standartNT: Gram.NonTerminal.StandardNT): Grammar.StandardNonTerminal =
            standartNT.lift match {
              case Gram.NonTerminal.BasicNT(_, reductions) =>
                Grammar.StandardNonTerminal.`:`(
                  reductions.toNonEmptyList.map {
                    _.toList.map(convertElement)
                  },
                )
              case Gram.NonTerminal.LiftNT(_, reductions) =>
                Grammar.StandardNonTerminal.^(
                  reductions.toNonEmptyList.map(convertIgnoredList),
                )
            }

          def convertListNT(listNT: Gram.NonTerminal.ListNT): Grammar.ListNonTerminal =
            listNT match {
              case Gram.NonTerminal.ListNT._1(listType, start) =>
                Grammar
                  .ListNonTerminal(
                    `type` = convertListType(listType),
                    start = convertIgnoredList(start),
                    repeat = None,
                  )
              case Gram.NonTerminal.ListNT._2(listType, start, _, repeat) =>
                Grammar
                  .ListNonTerminal(
                    `type` = convertListType(listType),
                    start = convertIgnoredList(start),
                    repeat = convertIgnoredList(repeat).some,
                  )
            }

          def convertAssocNT(assocNT: Gram.NonTerminal.AssocNT): Grammar.AssocNonTerminal = {
            def convertAssocType(assocType: Gram.Tok.assocType): Marked[Grammar.AssocNonTerminal.Type] = {
              assocType.text match {
                case "<" => Grammar.AssocNonTerminal.Type.Left
                case ">" => Grammar.AssocNonTerminal.Type.Right
                case _   => ???
              }
            }.marked(assocType.span)

            Grammar.AssocNonTerminal(
              assocs = assocNT._1.toNonEmptyList.map { t =>
                (
                  convertAssocType(t._0),
                  convertElement(t._1),
                )
              },
              base = convertStandardNT(assocNT._2),
            )
          }

          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(nt._0.markedText),
            nt = nt._1 match {
              case Gram.NonTerminal.NTBody._1(standardNT) => convertStandardNT(standardNT)
              case Gram.NonTerminal.NTBody._2(listNT)     => convertListNT(listNT)
              case Gram.NonTerminal.NTBody._3(assocNT)    => convertAssocNT(assocNT)
            },
          )
        }

        for {
          ntRoot <- Gram.parser.parse(grammarSource)
        } yield Grammar(
          startNt = ntRoot._1.markedText,
          nts = ntRoot._2.toNonEmptyList.map(convertNt).toList, // TODO (KR) : Should be NonEmptyList
        )
      }

      for {
        tmp1 <- ado[IO].join(
          Source.fromFile(lexerFile),
          Source.fromFile(grammarFile),
        )
        (lexerSource, grammarSource) = tmp1

        buildInputA = for {
          tmp2 <- ado[Attempt].join(
            convertLexer(lexerSource),
            convertGrammar(grammarSource),
          )
          (cLexer, cGrammar) = tmp2
          buildInput = BuildInput(name = baseName, lexer = cLexer, grammar = cGrammar)
        } yield buildInput

        _ <- buildInputA match {
          case Alive(buildInput) =>
            val buildOutputA = Build.buildOutput(buildInput)
            ado[IO].join(
              debugOutputDir match {
                case Some(debugOutputDir) =>
                  OutputDebug.outputDebug(buildInput, buildOutputA, new File(debugOutputDir, pkg.mkString("/")).some)
                case None =>
                  ().pure[IO]
              },
              buildOutputA match {
                case Right(buildOutput) =>
                  val outputFile = new File(new File(outputDir, pkg.mkString("/")), s"$baseName.scala")
                  val outputText = Build.outputToString(pkg, buildOutput).toString("  ")
                  for {
                    _ <- Maybe(outputFile.getParentFile).map(_.mkdirs.pure[IO]).traverse
                    _ <- IO.writeFile(outputFile, outputText)
                  } yield ()
                case Left(partialBuildOutput) =>
                  partialBuildOutput.toBuildOutput match {
                    case Alive(_) =>
                      IO.error(Message("This should not be possible..."))
                    case Dead(errors) =>
                      // TODO (KR) : Display errors properly (Group by source)
                      for {
                        _ <- logger(L(errors.map(L.log.fatal(_)), L.break()))
                        _ <- IO.error(Message(s"Failed to generate: $scopedName")): IO[Unit]
                      } yield ()
                  }
              },
            )
          case Dead(errors) =>
            // TODO (KR) : Display errors properly (Group by source)
            for {
              _ <- logger(L(errors.map(L.log.fatal(_)), L.break()))
              _ <- IO.error(Message(s"Failed to generate: $scopedName")): IO[Unit]
            } yield ()
        }
      } yield ()
    }

  }

  def main(args: Array[String]): Unit = {
    val fnaeReg = "^([^.]+)\\.([^.]+)$".r
    def fileNameAndExt(file: File): ?[(String, String)] = {
      val name = file.getName
      name match {
        case fnaeReg(baseName, ext) => (baseName, ext).pure[?]
        case _                      => ?.dead(Message(s"Invalid file name: $name"))
      }
    }

    def makePair(
        pkg: List[String],
        name: String,
        map: Map[String, File],
        nameMap: Maybe[String],
    ): (Maybe[Pair], Maybe[Logger.Event]) = {
      val lexer = map.get(LexerExtension).toMaybe
      val grammar = map.get(GrammarExtension).toMaybe
      // TODO (KR) :
      val extras = map.keySet &~ Set(LexerExtension, GrammarExtension)

      lazy val scopedName = s"${pkg.mkString(".")}.$name"

      (lexer, grammar) match {
        case (Some(lexer), Some(grammar)) =>
          (
            Pair(
              pkg = pkg,
              baseName = nameMap.cata(_.replaceAll("%n", name), name),
              lexerFile = lexer,
              grammarFile = grammar,
            ).some,
            None,
          )
        case (Some(_), None) =>
          (
            None,
            L.log.warning(s"$scopedName: Found lexer but not grammar").some,
          )
        case (None, Some(_)) =>
          (
            None,
            L.log.warning(s"$scopedName: Found grammar but not lexer").some,
          )
        case (None, None) =>
          (
            None,
            L.log.warning(s"$scopedName: Didn't find lexer or grammar").some,
          )
      }
    }

    // =====| generate |=====

    val generate = {
      // --- all ---
      val all = {
        final class Conf(args: Seq[String]) extends Executable.Conf(args) {
          helpWidth(125)

          version(s"Slyce v${slyce.BuildInfo.version}")

          banner {
            s"""
              |Recursively search for matching [baseName].(slf/sgf) file pairs.
              |Generates parsing files in a matching structure in the output directory.
              |
              |Example: -i src/main/slyce -o src/main/scala
              | .
              | |-- src
              | |   |-- main
              | |   |   |-- slyce
              | |   |   |   |-- myPkg
              | |   |   |   |   |-- test.slf
              | |   |   |   |   |-- test.sgf
              | |   |   |-- scala
              | |   |   |   |-- myPkg
              | |   |   |   |   |-- test.scala
              |""".stripMargin
          }

          val inputDir: ScallopOption[File] =
            opt(
              required = true,
              descr = "Directory to recursively search for .slf & .sgf files",
            )
          val outputDir: ScallopOption[File] =
            opt(
              required = true,
              descr = {
                """Directory to output generated files
                  |(in same tree structure as inputDir)""".stripMargin
              },
            )
          val debugOutputDir: ScallopOption[File] = opt(descr = "Write html debug output for generation")

          val nameMap: ScallopOption[String] = opt(
            descr = {
              """Map generated filename
                |ex: ex.(slf/sgf) => -n "%n2" => ex2.scala
                |""".stripMargin
            },
          )

          verify()
        }
        object Conf extends Executable.ConfBuilder(new Conf(_))

        Executable
          .fromConf(Conf) { (logger, conf) =>
            def findPairs(dir: File, rPkg: List[String]): IO[List[Pair]] =
              for {
                exists <- dir.exists.pure[IO]
                _ <- exists ? ().pure[IO] | IO.error(Message(s"Directory does not exist: $dir"))
                isDir <- dir.isDirectory.pure[IO]
                _ <- isDir ? ().pure[IO] | IO.error(Message(s"Not a directory: $dir"))
                myPkg = rPkg.reverse

                cf <- dir.listFiles.pure[IO].map(_.toList)
                childFilePairs <- cf.map(f => f.isDirectory.pure[IO].map((f, _))).traverse
                (childDirs, childFiles) = childFilePairs.partitionMap {
                  case (file, isDir) => isDir ? scala.Left(file) | scala.Right(file)
                }

                childFilesWithExtras <- childFiles.map(f => fileNameAndExt(f).map((f, _))).traverse.toIO
                groupedFileMap =
                  childFilesWithExtras
                    .groupMap(_._2._1)(p => (p._2._2, p._1))
                    .toList
                    .map { case (k, v) => k -> v.toMap }
                pairTs = groupedFileMap.map { case (name, map) => makePair(myPkg, name, map, conf.nameMap.toOption.toMaybe) }
                pairs = pairTs.flatMap(_._1)
                logEvents = pairTs.flatMap(_._2)

                _ <- logEvents.nonEmpty ? logger(L(logEvents, L.break())) | ().pure[IO]

                fromChildDirs <- childDirs.map(d => findPairs(d, d.getName :: rPkg)).traverse
              } yield (pairs :: fromChildDirs).flatten

            for {
              _ <- logger(
                L(
                  L.log.info("=====| Generate - all |====="),
                  L.break(),
                  // keep ?
                  L.log.info(s" input-dir: ${conf.inputDir()}"),
                  L.log.info(s"output-dir: ${conf.outputDir()}"),
                  L.break(),
                ),
              )

              pairs <- findPairs(conf.inputDir(), Nil)
              _ <- logger(
                L(
                  L.log.info(s"Found ${pairs.size} source(s):"),
                  L.indented(
                    pairs.map(p => L.log.info(s"${p.pkg.mkString(".")}.${p.baseName}")),
                  ),
                  L.break(),
                ),
              )

              _ <- pairs.map(_.generate(logger, conf.outputDir(), conf.debugOutputDir.toOption.toMaybe)).traverse
            } yield ()
          }
      }

      // --- one ---

      val one = {
        final class Conf(args: Seq[String]) extends Executable.Conf(args) {
          // TODO (KR) :

          verify()
        }
        object Conf extends Executable.ConfBuilder(new Conf(_))

        Executable
          .fromConf(Conf) { (logger, conf) =>
            for {
              _ <- logger(
                L(
                  L.log.info("=====| Generate - one |====="),
                  L.break(),
                ),
              )
              _ <- IO.error(Message("TODO")): IO[Unit]
            } yield ()
          }
      }

      // --- generate ---

      Executable
        .fromSubCommands(
          "all" -> all,
          // TODO (KR) : Implement
          // "one" -> one,
        )
    }

    // =====| everything |=====

    Executable
      .fromSubCommands(
        "generate" -> generate,
      )(args)
      .runSyncOrExit(None)
  }

}
/*
  -o, --output-dir  <arg>         Directory to output generated files in
                                  In same tree structure as inputDir
 */
