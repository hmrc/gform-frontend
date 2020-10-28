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

import scala.util.Try
import uk.gov.hmrc.gform.gform.FormComponentUpdater
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ SmartString, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait FormModelExpander[T <: PageMode] {
  def lift(page: Page[Basic], data: VariadicFormData[SourceOrigin.OutOfDate]): Page[T]
  def liftRepeating(section: Section.RepeatingPage, data: VariadicFormData[SourceOrigin.OutOfDate]): List[Singleton[T]]
}

object FormModelExpander {

  implicit def dataExpanded[D <: DataOrigin](implicit fmvo: FormModelVisibilityOptics[D]) =
    new FormModelExpander[DataExpanded] {
      def lift(page: Page[Basic], data: VariadicFormData[SourceOrigin.OutOfDate]): Page[DataExpanded] = {
        val expanded = page.fields.flatMap {
          case fc @ IsRevealingChoice(_) => fc :: Nil
          case fc @ IsGroup(group)       => ExpandUtils.expandGroup(fc, group, data)
          case otherwise                 => otherwise :: Nil
        }
        page.copy(fields = expanded).asInstanceOf[Page[DataExpanded]]
      }

      // Perfect we have access to FormModelVisibilityOptics, so we can evaluate 'section.repeats' expression
      def liftRepeating(
        section: Section.RepeatingPage,
        data: VariadicFormData[SourceOrigin.OutOfDate]): List[Singleton[DataExpanded]] = {
        val repeats = section.repeats
        val r: String = fmvo.eval(repeats)
        val repeatCount = Try(r.toInt).getOrElse(1)
        (1 to repeatCount).toList.map { index =>
          val pageBasic: Page[Basic] = mkSingleton2(section.page, index)(section)
          Singleton(pageBasic.asInstanceOf[Page[DataExpanded]], section)
        }
      }
    }

  implicit val interim = new FormModelExpander[Interim] {
    def lift(page: Page[Basic], data: VariadicFormData[SourceOrigin.OutOfDate]): Page[Interim] = {
      val expanded = page.fields.flatMap {
        case fc @ IsRevealingChoice(rc) =>
          fc.copy(`type` = RevealingChoice.slice(fc.id)(data)(rc)) :: Nil
        case fc @ IsGroup(group) => ExpandUtils.expandGroup(fc, group, data)
        case otherwise           => otherwise :: Nil
      }
      page.copy(fields = expanded).asInstanceOf[Page[Interim]]
    }
    // Expand by data, we don't know value of 'section.repeats' expression here
    def liftRepeating(
      section: Section.RepeatingPage,
      data: VariadicFormData[SourceOrigin.OutOfDate]): List[Singleton[Interim]] = {
      val baseIds: Set[BaseComponentId] = {
        section.allIds.map(_.baseComponentId)
      }.toSet

      val indexes: Set[Int] = data
        .keySet()
        .collect {
          // format: off
          case ModelComponentId.Pure(IndexedComponentId.Indexed(baseComponentId, index))      if baseIds(baseComponentId) => index
          case ModelComponentId.Atomic(IndexedComponentId.Indexed(baseComponentId, index), _) if baseIds(baseComponentId) => index
          // format: on
        }
        .toSet

      val repeatCount = if (indexes.isEmpty) 1 else indexes.max

      (1 to repeatCount).toList.map { index =>
        val pageBasic: Page[Basic] = mkSingleton2(section.page, index)(section)
        Singleton(pageBasic.asInstanceOf[Page[Interim]], section)
      }
    }
  }

  implicit val dependencyGraphVerification = new FormModelExpander[DependencyGraphVerification] {
    def lift(page: Page[Basic], data: VariadicFormData[SourceOrigin.OutOfDate]): Page[DependencyGraphVerification] = {
      val expanded = page.fields.flatMap {
        case fc @ IsRevealingChoice(revealingChoice) => fc :: revealingChoice.options.toList.flatMap(_.revealingFields)
        case fc @ IsGroup(group)                     => fc :: group.fields
        case otherwise                               => otherwise :: Nil
      }
      page.copy(fields = expanded).asInstanceOf[Page[DependencyGraphVerification]]
    }
    def liftRepeating(
      section: Section.RepeatingPage,
      data: VariadicFormData[SourceOrigin.OutOfDate]): List[Singleton[DependencyGraphVerification]] = {
      val pageBasic = mkSingleton2(section.page, 1)(section)
      Singleton(pageBasic.asInstanceOf[Page[DependencyGraphVerification]], section) :: Nil
    }

  }

  private def mkSingleton2(page: Page[Basic], index: Int): Section.RepeatingPage => Page[Basic] =
    source => {
      val expand: SmartString => SmartString = _.expand(index, source.allIds).replace("$n", index.toString)
      page.copy(
        title = expand(page.title),
        description = page.description.map(expand),
        shortName = page.shortName.map(expand),
        progressIndicator = page.progressIndicator.map(expand),
        continueLabel = page.continueLabel.map(expand),
        fields = page.fields.map { field =>
          new FormComponentUpdater(field, index, source.allIds).updatedWithId
        },
        instruction = page.instruction.map(i => i.copy(expand(i.name)))
      )
    }

}
