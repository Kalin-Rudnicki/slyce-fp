package slyce.core

import slyce.core.Marked.Implicits._

trait Token {

  val tokName: String
  def text: String
  def span: Span

  def markedText: Marked[String] =
    text.marked(span)

}
