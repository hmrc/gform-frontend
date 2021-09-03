/*
 * Copyright 2021 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.auth.models

import play.api.mvc.Call

sealed trait AuthResult
final case class AuthSuccessful(retrievals: MaterialisedRetrievals, role: Role) extends AuthResult
final case class AuthRedirect(loginUrl: String, flashing: Seq[(String, String)] = Seq.empty) extends AuthResult
final case class AuthAnonymousSession(redirectUrl: Call) extends AuthResult
final case class AuthEmailRedirect(redirectUrl: Call) extends AuthResult
final case class AuthRedirectFlashingFormName(loginUrl: String) extends AuthResult
final case class AuthBlocked(message: String) extends AuthResult
final case class AuthForbidden(message: String) extends AuthResult
final case class AuthCustomRedirect(redirectUrl: Call) extends AuthResult
