package slyce.parse

sealed trait Expression[+Operand, +Operator]
object Expression {
  final case class Node[+Operand, +Operator](
      left: Operand,
      op: Operator,
      right: Operand,
  ) extends Expression[Operand, Operator]
  final case class Leaf[+Operand](operand: Operand) extends Expression[Operand, Nothing]
}
