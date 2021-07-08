package slyce.parse

trait Grammar[Tok, Nt, NtRoot <: Nt] {

  def buildTree(tokens: List[Tok]): Attempt[NtRoot]

}
