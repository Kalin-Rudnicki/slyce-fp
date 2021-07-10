package slyce.parse

sealed trait Expression[+Operand, +Operator]
object Expression {
  final case class Node[+Operand, +Operator](
      left: Expression[Operand, Operator],
      op: Operator,
      right: Expression[Operand, Operator],
  ) extends Expression[Operand, Operator]
  final case class Leaf[+Operand](operand: Operand) extends Expression[Operand, Nothing]
}
