/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.controllers

import play.api.mvc.Session
import uk.gov.hmrc.gform.gformbackend.model._
import uk.gov.hmrc.gform.models.UserId

object GformSession {

  implicit class SessionOps(session: Session) {

    def getFormId: Option[FormId] = session.data.get(formId).map(FormId.apply)
    def putFormId(v: FormId): Session = session + (formId -> v.value)
    def removeFormId: Session = session.-(formId)

    def getUserId: Option[UserId] = session.data.get(userId).map(UserId.apply)
    def putUserId(v: UserId): Session = session + (userId -> v.value)
    def removeUserId: Session = session.-(userId)

    def getVersion: Option[Version] = session.data.get(version).map(Version.apply)
    def putVersion(v: Version): Session = session + (version -> v.value)
    def removeVersion: Session = session.-(version)

    def getFormTypeId: Option[FormTypeId] = session.data.get(formTypeId).map(FormTypeId.apply)
    def putFormTypeId(v: FormTypeId): Session = session + (formTypeId -> v.value)
    def removeFormTypeId: Session = session.-(formTypeId)

    def getEnvelopeId: Option[EnvelopeId] = session.data.get(envelopeId).map(EnvelopeId.apply)
    def putEnvelopeId(v: EnvelopeId): Session = session + (envelopeId -> v.value)
    def removeEnvelopId: Session = session.-(envelopeId)

    def getSectionNumber: Option[SectionNumber] = session.data.get(sectionNumber).map(_.toInt).map(SectionNumber.apply)
    def putSectionNumber(v: SectionNumber): Session = session + (sectionNumber -> v.value.toString)
    def removeSectionNumber: Session = session.-(sectionNumber)
  }

  private lazy val formId = s"$prefix.formId"
  private lazy val userId = s"$prefix.userId"
  private lazy val envelopeId = s"$prefix.envelopeId"
  private lazy val version = s"$prefix.formVersion"
  private lazy val formTypeId = s"$prefix.formTypeId"
  private lazy val sectionNumber = s"$prefix.sectionNumber"

  private lazy val prefix = "gform"
}
