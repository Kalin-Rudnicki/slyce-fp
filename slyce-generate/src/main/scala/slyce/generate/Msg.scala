package slyce.generate

import klib.fp.types._

final case class Msg(
    messageType: Maybe[Msg.Type],
    message: String,
)

object Msg {

  sealed trait Type
  object Type {
    case object UserError extends Type
  }

  def apply(message: String): Msg =
    Msg(
      None,
      message,
    )

  // =====| MessageType helpers |=====

  def userError(message: String): Msg =
    Msg(
      Some(Type.UserError),
      message,
    )

}
