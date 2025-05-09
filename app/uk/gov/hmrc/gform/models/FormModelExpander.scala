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

import cats.data.NonEmptyList
import play.api.i18n.Messages
import uk.gov.hmrc.gform.gform.FormComponentUpdater
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ SmartString, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait FormModelExpander[T <: PageMode] {
  def lift(page: Page[Basic], data: VariadicFormData[SourceOrigin.OutOfDate]): Page[T]
  def liftRepeating(
    section: Section.RepeatingPage,
    templateSectionIndex: TemplateSectionIndex,
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ): Option[Bracket.RepeatingPage[T]]
}

object FormModelExpander {

  private val repeatsLimit =
    99 // Repeated section must be limited since repeatsMax expression has not been validated at this point

  implicit def dataExpanded[D <: DataOrigin](implicit
    fmvo: FormModelVisibilityOptics[D],
    messages: Messages
  ): FormModelExpander[DataExpanded] =
    new FormModelExpander[DataExpanded] {
      def lift(page: Page[Basic], data: VariadicFormData[SourceOrigin.OutOfDate]): Page[DataExpanded] = {
        val expanded = page.fields.flatMap {
          case fc @ IsChoice(choice)     => OptionDataUtils.expand(fc, choice) :: Nil
          case fc @ IsTableComp(table)   => TableUtils.expand(fc, table) :: Nil
          case fc @ IsRevealingChoice(_) => fc :: Nil
          case fc @ IsGroup(group)       => ExpandUtils.expandGroup(fc, group, data)
          case otherwise                 => otherwise :: Nil
        }
        page.copy(fields = expanded).asInstanceOf[Page[DataExpanded]]
      }

      // Perfect we have access to FormModelVisibilityOptics, so we can evaluate 'section.repeats' expression
      def liftRepeating(
        section: Section.RepeatingPage,
        templateSectionIndex: TemplateSectionIndex,
        data: VariadicFormData[SourceOrigin.OutOfDate]
      ): Option[Bracket.RepeatingPage[DataExpanded]] = {
        val repeats = section.repeats
        val bdRepeats: Option[BigDecimal] = fmvo.evalAndApplyTypeInfoFirst(repeats).numberRepresentation
        val repeatCount = Math.min(bdRepeats.fold(0)(_.toInt), repeatsLimit)
        val singletons = (1 to repeatCount).toList.map { index =>
          val pageBasic: Page[Basic] = mkSingleton(section.page, index)(section)
          Singleton(pageBasic.asInstanceOf[Page[DataExpanded]])
        }
        NonEmptyList
          .fromList(singletons)
          .map(xs =>
            Bracket.RepeatingPage(
              xs.zipWithIndex.map { case (s, i) =>
                SingletonWithNumber(s, SectionNumber.Classic.RepeatedPage(templateSectionIndex, i))
              },
              section
            )
          )
      }
    }

  implicit val interim: FormModelExpander[Interim] = new FormModelExpander[Interim] {
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
      templateSectionIndex: TemplateSectionIndex,
      data: VariadicFormData[SourceOrigin.OutOfDate]
    ): Option[Bracket.RepeatingPage[Interim]] = {
      val baseIds: Set[BaseComponentId] =
        section.allIds.map(_.baseComponentId).toSet

      val indexes: collection.Set[Int] = data
        .keySet()
        .collect {
          // format: off
          case ModelComponentId.Pure(IndexedComponentId.Indexed(baseComponentId, index))      if baseIds(baseComponentId) => index
          case ModelComponentId.Atomic(IndexedComponentId.Indexed(baseComponentId, index), _) if baseIds(baseComponentId) => index
          // format: on
        }

      val repeatCount = if (indexes.isEmpty) 1 else indexes.max

      val singletons = (1 to repeatCount).toList.map { index =>
        val pageBasic: Page[Basic] = mkSingleton(section.page, index)(section)
        Singleton(pageBasic.asInstanceOf[Page[Interim]])
      }

      NonEmptyList
        .fromList(singletons)
        .map(xs =>
          Bracket.RepeatingPage(
            xs.zipWithIndex.map { case (s, i) =>
              SingletonWithNumber(s, SectionNumber.Classic.RepeatedPage(templateSectionIndex, i))
            },
            section
          )
        )
    }
  }

  implicit val dependencyGraphVerification: FormModelExpander[DependencyGraphVerification] =
    new FormModelExpander[DependencyGraphVerification] {
      def lift(
        page: Page[Basic],
        data: VariadicFormData[SourceOrigin.OutOfDate]
      ): Page[DependencyGraphVerification] = {
        val expanded = page.fields.flatMap {
          case fc @ IsRevealingChoice(revealingChoice) =>
            fc :: revealingChoice.options.flatMap(_.revealingFields)
          case fc @ IsGroup(group) => fc :: group.fields
          case otherwise           => otherwise :: Nil
        }
        page.copy(fields = expanded).asInstanceOf[Page[DependencyGraphVerification]]
      }
      def liftRepeating(
        section: Section.RepeatingPage,
        templateSectionIndex: TemplateSectionIndex,
        data: VariadicFormData[SourceOrigin.OutOfDate]
      ): Option[Bracket.RepeatingPage[DependencyGraphVerification]] = {
        val pageBasic = mkSingleton(section.page, 1)(section)
        val singletons = NonEmptyList.one(
          Singleton(pageBasic.asInstanceOf[Page[DependencyGraphVerification]])
        )
        Some(
          Bracket.RepeatingPage(
            singletons.zipWithIndex.map { case (s, i) =>
              SingletonWithNumber(s, SectionNumber.Classic.RepeatedPage(templateSectionIndex, i))
            },
            section
          )
        )
      }

    }

  private def mkSingleton(page: Page[Basic], index: Int): Section.RepeatingPage => Page[Basic] =
    source => {
      val expand: SmartString => SmartString = _.expand(index, source.allIds).replace("$n", index.toString)
      page.copy(
        title = expand(page.title),
        id = page.id.map(id => id.withIndex(index)),
        description = page.description.map(expand),
        shortName = page.shortName.map(expand),
        caption = page.caption.map(expand),
        continueLabel = page.continueLabel.map(expand),
        fields = page.fields.map { field =>
          new FormComponentUpdater(field, index, source.allIds).updatedWithId
        },
        instruction = page.instruction.map(i => i.copy(name = i.name.map(expand)))
      )
    }

}
