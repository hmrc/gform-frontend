/*
 * Copyright 2023 HM Revenue & Customs
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

import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, HasEnrolmentSection, Section }

sealed trait SectionSelectorType extends Product with Serializable

object SectionSelectorType {
  trait EnrolmentOnly extends SectionSelectorType
  trait Normal extends SectionSelectorType
  trait WithDeclaration extends SectionSelectorType
  trait WithAcknowledgement extends SectionSelectorType
}

trait SectionSelector[T <: SectionSelectorType] {
  def getSections(formTemplate: FormTemplate): AllSections
}

object SectionSelector {
  implicit val normal: SectionSelector[SectionSelectorType.Normal] = new SectionSelector[SectionSelectorType.Normal] {
    def getSections(formTemplate: FormTemplate): AllSections = formTemplate.formKind.allSections
  }

  implicit val withDeclaration: SectionSelector[SectionSelectorType.WithDeclaration] =
    new SectionSelector[SectionSelectorType.WithDeclaration] {

      def getSections(formTemplate: FormTemplate): AllSections = {
        val destinationSections: List[Section] = formTemplate.destinations.fold(destinationList =>
          destinationList.declarationSection.toList.map(_.toSection)
        )(destinationPrint => Nil)

        formTemplate.formKind.allSections + destinationSections
      }
    }

  implicit val enrolmentOnly: SectionSelector[SectionSelectorType.EnrolmentOnly] =
    new SectionSelector[SectionSelectorType.EnrolmentOnly] {

      def getSections(formTemplate: FormTemplate): AllSections =
        formTemplate.authConfig match {
          case HasEnrolmentSection((_, enrolmentSection, _, _)) =>
            AllSections.Classic(Nil, enrolmentSection.toSection :: Nil)
          case _ => AllSections.Classic(Nil, Nil)
        }
    }

  implicit val withAcknowledgement: SectionSelector[SectionSelectorType.WithAcknowledgement] =
    new SectionSelector[SectionSelectorType.WithAcknowledgement] {

      def getSections(formTemplate: FormTemplate): AllSections = {
        val destinationSections: List[Section] = formTemplate.destinations.fold(destinationList =>
          destinationList.declarationSection.toList.map(_.toSection) ++ List(
            destinationList.acknowledgementSection.toSection
          )
        )(destinationPrint => Nil)
        formTemplate.formKind.allSections + destinationSections
      }
    }
}
