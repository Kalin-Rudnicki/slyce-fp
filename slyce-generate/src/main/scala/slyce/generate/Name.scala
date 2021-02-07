package slyce.generate

import slyce.generate.input.Grammar.Identifier

sealed trait Name

object Name {

  private val StartIdx = 1

  final case class Named private (name: Identifier.NonTerminal, idx: Int) extends Name {

    def next: Named =
      Named(name, idx + 1)

  }
  object Named {
    def apply(name: Identifier.NonTerminal): Named =
      Named(name, StartIdx)
  }

  final case class AnonList private (objId: Object, idx: Int) extends Name {

    def next: AnonList =
      AnonList(objId, idx + 1)

  }
  object AnonList {

    def apply(): AnonList =
      AnonList(new Object, StartIdx)

  }

  final case class Optional(identifier: Identifier) extends Name

}
