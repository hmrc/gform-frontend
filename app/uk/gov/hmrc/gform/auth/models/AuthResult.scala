package uk.gov.hmrc.gform.auth.models

sealed trait AuthResult

object UnAuthenticated extends AuthResult
object NeedsAuthenticated extends AuthResult
object Authenticated extends AuthResult