package slyce.test.examples.calc

import scala.annotation.tailrec

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import klib.utils.Logger.{helpers => L}
import slyce.core._
import slyce.parse._
import slyce.test.examples._
import slyce.test.examples.calc.calc._

object Parse {

  lazy val executable: Executable =
    debugParse(parser) { (logger, root) =>
      def simplifyExpr(expr: NonTerminal.Expr): Expression[NonTerminal.Constant \/ Tok.variable, String] = {
        def expr1(expr: NonTerminal.Expr1): Expression[NonTerminal.Constant \/ Tok.variable, String] =
          expr match {
            case NonTerminal.Expr1._1(left, op, right) => Expression.Node(expr1(left), op.text, expr2(right))
            case NonTerminal.Expr1._2(expr)            => expr2(expr)
          }
        def expr2(expr: NonTerminal.Expr2): Expression[NonTerminal.Constant \/ Tok.variable, String] =
          expr match {
            case NonTerminal.Expr2._1(left, op, right) => Expression.Node(expr2(left), op.text, expr3(right))
            case NonTerminal.Expr2._2(expr)            => expr3(expr)
          }
        def expr3(expr: NonTerminal.Expr3): Expression[NonTerminal.Constant \/ Tok.variable, String] =
          expr match {
            case NonTerminal.Expr3._1(left, op, right) => Expression.Node(expr4(left), op.text, expr3(right))
            case NonTerminal.Expr3._2(expr)            => expr4(expr)
          }
        def expr4(expr: NonTerminal.Expr4): Expression[NonTerminal.Constant \/ Tok.variable, String] =
          expr match {
            case NonTerminal.Expr4._1(variable)   => Expression.Leaf(variable.right)
            case NonTerminal.Expr4._2(constant)   => Expression.Leaf(constant.left)
            case NonTerminal.Expr4._3(_, expr, _) => expr1(expr)
          }

        expr1(expr)
      }

      @tailrec
      def eval(lines: List[NonTerminal.Line], variables: Map[String, Int]): Unit = {
        def evalExpr(expr: Expression[NonTerminal.Constant \/ Tok.variable, String]): String \/ Int =
          expr match {
            case Expression.Node(left, op, right) =>
              for {
                leftV <- evalExpr(left)
                rightV <- evalExpr(right)
                opF <- op match {
                  case "+" => { (a: Int, b: Int) => (a + b).right }.right
                  case "-" => { (a: Int, b: Int) => (a - b).right }.right
                  case "*" => { (a: Int, b: Int) => (a * b).right }.right
                  case "/" => { (a: Int, b: Int) => (b == 0) ? "Div/0".left | (a / b).right }.right
                  case "^" => { (a: Int, b: Int) => Math.pow(a, b).toInt.right }.right
                  case _   => s"Unknown operator: ${op.unesc}".left
                }
                res <- opF(leftV, rightV)
              } yield res
            case Expression.Leaf(operand) =>
              operand match {
                case Right(variable) =>
                  variables.get(variable.text).toMaybe match {
                    case Some(value) => value.right
                    case None        => s"Unknown variable: ${variable.text.unesc}".left
                  }
                case Left(constant) =>
                  constant.lift match {
                    case Tok.float(text, _) => ??? // TODO (KR) :
                    case Tok.int(text, _)   => text.toInt.right
                  }
              }
          }

        lines match {
          case head :: tail =>
            head match {
              case NonTerminal.Line._1(NonTerminal.Assign(variable, _, expr)) =>
                evalExpr(simplifyExpr(expr)) match {
                  case Left(error) =>
                    println(s"[error] $error")
                  case Right(value) =>
                    eval(tail, variables.updated(variable.text, value))
                }
              case NonTerminal.Line._2(expr, show) =>
                evalExpr(simplifyExpr(expr)) match {
                  case Left(error) =>
                    println(s"[error] $error")
                  case Right(value) =>
                    if (show.toMaybe.nonEmpty)
                      println(s"[value] $value")
                    eval(tail, variables)
                }
            }
          case Nil =>
        }
      }

      for {
        _ <- logger(L.log.info("SUCCESS!!!!!"))
        lines = root.toList
        _ = eval(lines, Map.empty)
      } yield ()
    }

}
