package slyce.test.examples.lexer

import slyce.test.examples._
import slyce.test.examples.lexer.lexer._

import klib.Implicits._
import klib.utils.Logger.{helpers => L}
import klib.utils._

object Parse {

  lazy val executable: Executable =
    debugParse(parser) { (logger, root) =>
      for {
        _ <- logger(L.log.info("SUCCESS!!!!!"))
      } yield ()
    }

}
