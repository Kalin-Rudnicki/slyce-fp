package slyce.test.examples.grammar

import scala.annotation.tailrec

import slyce.parse._
import slyce.test.examples._
import slyce.test.examples.grammar.grammar._

import klib.Implicits._
import klib.fp.types._
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
