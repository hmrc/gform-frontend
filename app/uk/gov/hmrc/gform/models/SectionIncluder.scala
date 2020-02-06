/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models

import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, Section }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.HasEnrolmentSection

sealed trait SectionSelectorType extends Product with Serializable

object SectionSelectorType {
  trait Enrolment extends SectionSelectorType
  trait Normal extends SectionSelectorType
  trait WithDeclaration extends SectionSelectorType
  trait WithAcknowledgement extends SectionSelectorType

}

trait SectionSelector[T <: SectionSelectorType] {
  def getSections(formTemplate: FormTemplate): List[Section]
}

object SectionSelector {
  implicit val normal = new SectionSelector[SectionSelectorType.Normal] {
    def getSections(formTemplate: FormTemplate): List[Section] = formTemplate.sections

  }

  implicit val withDeclaration = new SectionSelector[SectionSelectorType.WithDeclaration] {

    def getSections(formTemplate: FormTemplate): List[Section] = {
      val destinationSections: List[Section] = formTemplate.destinations.fold(destinationList =>
        destinationList.declarationSection.toSection :: Nil)(destinationPrint => Nil)

      formTemplate.sections ::: destinationSections
    }

  }

  implicit val enrolmentOnly = new SectionSelector[SectionSelectorType.Enrolment] {

    def getSections(formTemplate: FormTemplate): List[Section] =
      formTemplate.authConfig match {
        case HasEnrolmentSection((_, enrolmentSection, _, _)) =>
          enrolmentSection.toSection :: Nil
        case _ => Nil
      }
  }

  implicit val withAcknowledgement = new SectionSelector[SectionSelectorType.WithAcknowledgement] {

    def getSections(formTemplate: FormTemplate): List[Section] = {
      val destinationSections: List[Section] = formTemplate.destinations.fold(
        destinationList =>
          destinationList.declarationSection.toSection ::
            destinationList.acknowledgementSection.toSection :: Nil)(destinationPrint => Nil)
      formTemplate.sections ::: destinationSections
    }

  }

}
