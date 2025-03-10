/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.retrieval

import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._
import uk.gov.hmrc.auth.core.CredentialRole
import uk.gov.hmrc.gform.auth.models.{ AuthenticatedRetrievals, EmailRetrievals }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.models.mappings.{ IRCT, IRSA, NINO, VRN }
import uk.gov.hmrc.gform.sharedmodel.AffinityGroup
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId

case class FormAuthRetrievals(
  _id: EnvelopeId,
  email: Option[String],
  emailLogin: Boolean,
  ggLogin: Boolean,
  payeNino: Option[String],
  ctUtr: Option[String],
  saUtr: Option[String],
  payeRef: Option[String],
  vrn: Option[String],
  affinityGroup: Option[AffinityGroup],
  credentialRole: Option[CredentialRole]
)

object FormAuthRetrievals {
  def fromCache(cache: AuthCacheWithForm): FormAuthRetrievals = {
    def optFromStr(s: String): Option[String] =
      if (s.isEmpty) {
        None
      } else {
        Some(s)
      }

    apply(
      _id = cache.form.envelopeId,
      email = optFromStr(cache.retrievals.getEmail.toString),
      emailLogin = cache.retrievals match {
        case _: EmailRetrievals => true
        case _                  => false
      },
      ggLogin = cache.retrievals match {
        case _: AuthenticatedRetrievals => true
        case _                          => false
      },
      payeNino = optFromStr(cache.retrievals.getTaxIdValue(NINO())),
      ctUtr = optFromStr(cache.retrievals.getTaxIdValue(IRCT())),
      saUtr = optFromStr(cache.retrievals.getTaxIdValue(IRSA())),
      payeRef = optFromStr(cache.retrievals.getPayeRef),
      vrn = optFromStr(cache.retrievals.getTaxIdValue(VRN())),
      affinityGroup = cache.retrievals.getAffinityGroup,
      credentialRole = cache.retrievals.getCredentialRole
    )
  }

  private def optionFormat[T: Format]: Format[Option[T]] = new Format[Option[T]] {
    override def reads(json: JsValue): JsResult[Option[T]] = json.validateOpt[T]

    override def writes(o: Option[T]): JsValue = o match {
      case Some(t) => implicitly[Writes[T]].writes(t)
      case None    => JsNull
    }
  }

  private val reads: Reads[FormAuthRetrievals] = (
    (EnvelopeId.oformat: Reads[EnvelopeId]) and
      (__ \ "email").readNullable[String] and
      (__ \ "emailLogin").read[Boolean] and
      (__ \ "ggLogin").read[Boolean] and
      (__ \ "payeNino").readNullable[String] and
      (__ \ "ctUtr").readNullable[String] and
      (__ \ "saUtr").readNullable[String] and
      (__ \ "payeRef").readNullable[String] and
      (__ \ "vrn").readNullable[String] and
      (__ \ "affinityGroup").readNullable[AffinityGroup] and
      (__ \ "credentialRole").readNullable[CredentialRole]
  )(FormAuthRetrievals.apply _)

  private val writes: Writes[FormAuthRetrievals] = Writes[FormAuthRetrievals](retrievals =>
    EnvelopeId.oformat.writes(retrievals._id) ++
      Json.obj("email" -> optionFormat[String].writes(retrievals.email)) ++
      Json.obj("emailLogin" -> retrievals.emailLogin) ++
      Json.obj("ggLogin" -> retrievals.ggLogin) ++
      Json.obj("payeNino" -> optionFormat[String].writes(retrievals.payeNino)) ++
      Json.obj("ctUtr" -> optionFormat[String].writes(retrievals.ctUtr)) ++
      Json.obj("saUtr" -> optionFormat[String].writes(retrievals.saUtr)) ++
      Json.obj("payeRef" -> optionFormat[String].writes(retrievals.payeRef)) ++
      Json.obj("vrn" -> optionFormat[String].writes(retrievals.vrn)) ++
      Json.obj("affinityGroup" -> optionFormat[AffinityGroup].writes(retrievals.affinityGroup)) ++
      Json.obj("credentialRole" -> optionFormat[CredentialRole].writes(retrievals.credentialRole))
  )

  implicit val format: Format[FormAuthRetrievals] = Format[FormAuthRetrievals](reads, writes)
}
