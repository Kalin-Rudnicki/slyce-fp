
# Grammar

```
Basic : a
      | b c
Lift  ^ d
      | e ^f
List1_1 + g
List1_2 + h . i
List1_3 + ^j k . l ^j     // must be the same lift
List2_1 * g               // same format as `+`
Assoc1  ~ <m
        | >n
        : o
        | p
Assoc2  ~ >q
        | >r
        ^ s
        | t
```

# Example: Calc

### Simplified:
```
Lines * ^Line ";"
Line  : variable "=" Expr
      | Expr
Expr  ~ >powOp
      | <multOp
      | <addOp
      ^ "(" ^Expr ")"
      | variable
      | int
```

### Expanded
```
Lines : Line ";" Lines
      |
Line  : variable "=" Expr
      | Expr
Expr1 : Expr2 powOp Expr1
      | Expr2
Expr2 : Expr2 multOp Expr3
      | Expr3
Expr3 : Expr3 addOp Expr4
      | Expr4
Expr4 : "(" Expr1 ")"
      | variable
      | int
```

### Scala:
```scala
object Terminal {
  final case class `;`()
  final case class `=`()
  final case class `(`()
  final case class `)`()
  final case class variable()
  final case class int()
  final case class powOp() extends 
  final case class multOp() extends 
  final case class addOp() extends 
}

sealed trait Lines {
  def simplifyList: List[Lines.ElementType] = ???
}
object Lines {
  final case class _1(_1: Line, _2: Terminal.`;`, _3: Lines) extends Lines
  case object _2 extends Lines
  
  type ElementType = Line
}

sealed trait Line
object Line {
  final case class _1(_1: Terminal.variable, _2: Terminal.`=`, _3: Expr) extends Line
  final case class _2(_1: Expr) extends Line
}

sealed trait Expr
object Expr {
  final case class Leaf(operand: Operand) extends Expr
  final case class Node(left: Expr, operator: Operator, right: Expr) extends Expr
  
  sealed trait Operator
  sealed trait Operand
}

sealed trait __Expr_1 {
  def simplifyAssoc: __Expr_1.AssocType = {
    def simplify_1(element: __Expr_1): Expr =
      element match {
        case __Expr_1._1(_1, _2, _3) =>
          Expr.Node(simplify_2(_1), _2, simplify_1(_3))
        case __Expr_1._2(_1) =>
          simplify_2(_1)
      }
    def simplify_2(element: __Expr_1): Expr =
      element match {
        case __Expr_2._1(_1, _2, _3) =>
          Expr.Node(simplify_2(_1), _2, simplify_3(_3))
        case __Expr_2._2(_1) =>
          simplify_3(_1)
      }
    def simplify_3(element: __Expr_1): Expr =
      element match {
        case __Expr_3._1(_1, _2, _3) =>
          Expr.Node(simplify_3(_1), _2, simplify_4(_3))
        case __Expr_3._2(_1) =>
          simplify_4(_1)
      }
    def simplify_4(element: __Expr_1): Expr =
      element match {
        case __Expr_4._1(_1, _2, _3) =>
          simplify_1(_2)
        case __Expr_4._2(_1) =>
          Expr.Leaf(_1)
        case __Expr_4._3(_1) =>
          Expr.Leaf(_1)
      }
    
    simplify_1(this)
  }
}
object __Expr_1 {
  final case class _1(_1: __Expr_2, _2: Terminal.powOp, _3: __Expr_1) extends __Expr_1
  final case class _2(_1: __Expr_2) extends __Expr_1
  
  type AssocType = Expr
}

sealed trait __Expr_2
object __Expr_2 {
  final case class _1(_1: __Expr_2, _2: Terminal.powOp, _3: __Expr_3) extends __Expr_2
  final case class _2(_1: __Expr_3) extends __Expr_2
}

sealed trait __Expr_3
object __Expr_3 {
  final case class _1(_1: __Expr_3, _2: Terminal.powOp, _3: __Expr_4) extends __Expr_3
  final case class _2(_1: __Expr_4) extends __Expr_3
}

sealed trait __Expr_4
object __Expr_4 {
  final case class _1(_1: Terminal.`(`, _2: __Expr_1, _3: Terminal.`)`) extends __Expr_4
  final case class _2(_1: Terminal.variable) extends __Expr_4
  final case class _3(_1: Terminal.int) extends __Expr_4
}
```
