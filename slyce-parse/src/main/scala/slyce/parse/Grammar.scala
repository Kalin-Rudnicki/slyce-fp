package slyce.parse

trait Grammar[Tok, Raw] {

  def buildTree(tokens: List[Tok]): Attempt[Raw]

}
