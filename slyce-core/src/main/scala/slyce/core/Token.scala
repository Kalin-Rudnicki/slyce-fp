package slyce.core

trait Token {
  val tokName: String
  def span: Span
}
