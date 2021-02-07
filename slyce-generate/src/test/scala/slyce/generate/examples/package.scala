package slyce.generate

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import slyce.generate._
import input._
import building._
import slyce.core._

package object examples {

  private type Res = Attempt[(Nfa, Dfa)]

  private def convertRes(logger: Logger, res: Res): ??[Unit] = {
    def convertMarkedMsg(mMsg: Marked[Msg]): Throwable =
      Message(mMsg.toString)

    // TODO (KR) :
    for {
      tmp1 <- res.mapWE(convertMarkedMsg, convertMarkedMsg).wrap[IO]
      (nfa, dfa) = tmp1

      _ <- logger() { src =>
        src.info("Modes:")
        src.indented() { src =>
          dfa.modeStarts.foreach {
            case (k, v) =>
              src.info(s"$k: ${dfa.stateId(v)}")
          }
        }

        src.info("States:")
        src.indented() { src =>
          dfa.states.toList.map(_._1).foreach { state =>
            src.info(s"${dfa.stateId(state)}:")
            src.indented() { src =>
              src.info("Transitions:")
              src.indented() { src =>
                state.transitions.foreach {
                  case (k, v) =>
                    src.info(s"${k.prettyChars} => ${v.map(v => dfa.stateId(v.value))}")
                }
              }
              src.info("ElseTransition:")
              src.indented() { src =>
                state.elseTransition.forEach { v =>
                  src.info(dfa.stateId(v.value))
                }
              }
              src.info("End:")
              src.indented() { src =>
                state.end.forEach { y =>
                  src.info(s"Yields (${y.yields.size}): ${y.yields.map(_.value).mkString(", ")}")
                  src.info(s"ToMode: ${y.toMode.value.map(v => dfa.stateId(v.value))}")
                }
              }
            }
          }
        }
      }.wrap
    } yield ()
  }

  // TODO (KR) : Will need something different once it comes to reading/writing files
  def makeExecutable(
      lexer: Lexer,
      // TODO (KR) : Grammar
  ): Executable = { (logger, _) => // TODO (KR) : args
    val res: Res =
      for {
        tmp1 <- for {
          nfa <- Nfa.fromLexer(lexer)
          dfa <- Dfa.fromNfa(nfa)
        } yield (
          nfa,
          dfa,
        )
        (nfa, dfa) = tmp1

        // TODO (KR) : Grammar
      } yield (nfa, dfa)

    convertRes(logger, res)
  }

}
