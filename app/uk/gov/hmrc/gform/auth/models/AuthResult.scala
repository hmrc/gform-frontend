/*
 * Copyright 2018 HM Revenue & Customs
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

sealed trait AuthResult
final case class GGAuthSuccessful(retrievals: MaterialisedRetrievals) extends AuthResult
final case class AuthenticationFailed(loginUrl: String) extends AuthResult
final object AuthenticationWhiteListFailed extends AuthResult
final case class AuthorisationFailed(errorUrl: String) extends AuthResult
final object EnrolmentRequired extends AuthResult
