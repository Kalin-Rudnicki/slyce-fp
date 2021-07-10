package slyce.test.examples.calc

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import klib.utils.Logger.{helpers => L}

import slyce.core._
import slyce.test.examples._
import slyce.test.examples.calc.calc._

object Parse {

  lazy val executable: Executable =
    debugParse(parser) { (logger, root) =>
      for {
        _ <- logger(L.log.info("SUCCESS!!!!!"))
      } yield ()
    }

}
