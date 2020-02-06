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

package uk.gov.hmrc.gform.gform

import cats.{ Monad, MonadError }
import cats.data.NonEmptyList
import cats.instances.option._
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.syntax.flatMap._
import cats.syntax.functor._
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId, MultiValueId }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.{ DestinationList, DestinationPrint }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormDataFieldNamePurpose
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue.{ ArrayNode, ObjectStructure, TextNode }
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ Field, FieldName, StructuredFormValue }

object StructuredFormDataBuilder {
  def apply[D <: DataOrigin, F[_]: Monad](
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    destinations: Destinations,
    lookupRegistry: LookupRegistry)(implicit l: LangADT, me: MonadError[F, Throwable]): F[ObjectStructure] =
    new StructuredFormDataBuilder[D, F](formModelVisibilityOptics, destinations, lookupRegistry)
      .build()
      .map(StructuredFormValue.ObjectStructure.apply)

}

class StructuredFormDataBuilder[D <: DataOrigin, F[_]: Monad](
  formModelVisibilityOptics: FormModelVisibilityOptics[D],
  destinations: Destinations,
  lookupRegistry: LookupRegistry)(implicit me: MonadError[F, Throwable]) {

  private val isMultiSelectionIds: Set[ModelComponentId] = formModelVisibilityOptics.formModel.allMultiSelectionIds

  def build()(implicit l: LangADT): F[List[Field]] =
    destinations match {
      case DestinationList(_, _, declarationSection) => buildSections
      case DestinationPrint(_, _, _)                 => List.empty[Field].pure[F]
    }

  private def buildSections(implicit l: LangADT): F[List[Field]] = {

    val (addToListFields, addToListMultiValueIds) = buildAddToList
    val (revealingChoiceFields, rcMultiValueIds) = buildRevealingChoice

    val addToListAndRcMultivalues = addToListMultiValueIds ++ rcMultiValueIds

    val allMultiValueIds: List[MultiValueId] = formModelVisibilityOptics.formModel.allMultiValueIds

    val multiValuesNotProcessedYet: List[MultiValueId] = allMultiValueIds.filterNot(addToListAndRcMultivalues.contains)

    val restOfTheFields: F[List[Field]] = buildMultiField(multiValuesNotProcessedYet, false)

    (addToListFields, revealingChoiceFields, restOfTheFields).mapN(_ ++ _ ++ _)

  }

  private def buildAddToList(implicit l: LangADT): (F[List[Field]], List[MultiValueId]) = {
    val addToLists: List[(AddToListId, List[MultiValueId])] = formModelVisibilityOptics.formModel.pages
      .map(
        _.fold(
          singleton => singleton.sourceIsAddToList.map(_.id -> singleton.allMultiValueIds)
        )(
          repeater => Some(repeater.source.id -> repeater.allMultiValueIds)
        ))
      .flatten

    val addToListMultiValueIds: List[MultiValueId] = addToLists.flatMap(_._2)

    val addToListsMap: Map[AddToListId, List[MultiValueId]] =
      addToLists.groupBy(_._1).mapValues(_.map(_._2)).mapValues(_.flatten)

    val multiValueIdByIndex: Map[AddToListId, Map[Int, List[MultiValueId]]] =
      addToListsMap.mapValues(_.groupBy(_.modelComponentId.indexedComponentId.maybeIndex.getOrElse(0)))

    val addToListFields: F[List[Field]] = {
      val allRevealingChoicesIds: List[(FormComponent, RevealingChoice)] =
        formModelVisibilityOptics.formModel.allFormComponents.collect {
          case fc @ IsRevealingChoice(revealingChoice) => fc -> revealingChoice
        }

      val rcModelComponentIds: List[(ModelComponentId, (FormComponent, RevealingChoice))] = allRevealingChoicesIds.map {
        case (fc, rc) => (fc.modelComponentId, (fc, rc))
      }
      multiValueIdByIndex.toList.traverse {
        case (addToListId, indexWithMultiValues) =>
          val objectStructuresF: F[List[ObjectStructure]] =
            indexWithMultiValues.toList.sortBy { case (index, _) => index }.traverse {
              case (_, multiValues) =>
                val modelComponentIds: Set[ModelComponentId] = multiValues.map(_.modelComponentId).toSet

                val addToListRevealingChoices: List[(ModelComponentId, (FormComponent, RevealingChoice))] =
                  rcModelComponentIds.filter {
                    case (modelComponentId, _) => modelComponentIds(modelComponentId)
                  }

                val rcPureIndexedIds: List[(IndexedComponentId.Indexed, (FormComponent, RevealingChoice))] =
                  addToListRevealingChoices.collect {
                    case (ModelComponentId.Pure(ic @ IndexedComponentId.Indexed(_, _)), fcRc) => ic -> fcRc
                  }

                val rcPureIndexedIdsMap
                  : Map[BaseComponentId, List[(IndexedComponentId.Indexed, (FormComponent, RevealingChoice))]] =
                  rcPureIndexedIds.groupBy(_._1.baseComponentId)

                val revealingChoiceFields: F[List[Field]] = rcPureIndexedIdsMap.toList
                  .traverse {
                    case (baseComponentId, xs) =>
                      val revealingChoices: List[(FormComponent, RevealingChoice)] = xs.map(_._2)
                      val objectStructuresF: F[List[ObjectStructure]] =
                        processRevealingChoices(revealingChoices, true)((a, b) => b)
                      objectStructuresF.map(_.headOption.map(objectStructure =>
                        Field(FieldName(baseComponentId.value), objectStructure, Map.empty)))
                  }
                  .map(_.flatten)

                val rcIndexedComponents: List[IndexedComponentId.Indexed] = rcPureIndexedIds.map(_._1)

                val rcComponentsMultiValues: List[MultiValueId] =
                  rcPureIndexedIds
                    .map(_._2._2)
                    .flatMap { revealedChoice =>
                      revealedChoice.options.flatMap(_.revealingFields).map(_.multiValueId)
                    }

                val multiValuesNoRc: List[MultiValueId] = multiValues.filterNot { multiValue =>
                  rcIndexedComponents.contains(multiValue.modelComponentId.indexedComponentId) ||
                  rcComponentsMultiValues.contains(multiValue)
                }

                val restOfTheFields: F[List[Field]] = buildMultiField(multiValuesNoRc, true)

                (restOfTheFields, revealingChoiceFields).mapN(_ ++ _).map(ObjectStructure(_))
            }

          objectStructuresF.map(objectStructures =>
            Field(FieldName(addToListId.formComponentId.value), ArrayNode(objectStructures), Map.empty))

      }
    }

    (addToListFields, addToListMultiValueIds)

  }

  private def buildRevealingChoice(implicit l: LangADT): (F[List[Field]], List[MultiValueId]) = {
    val revealingChoices: List[(FormComponent, RevealingChoice)] =
      formModelVisibilityOptics.formModel.filter(!_.sourceIsAddToList.isDefined).allFormComponents.collect {
        case fc @ IsRevealingChoice(revealingChoice) => fc -> revealingChoice
      }

    val rcModelComponentIds: List[(ModelComponentId, (FormComponent, RevealingChoice))] = revealingChoices.map {
      case (fc, rc) => (fc.modelComponentId, (fc, rc))
    }

    val rcPurePureIds: List[(IndexedComponentId.Pure, (FormComponent, RevealingChoice))] = rcModelComponentIds.collect {
      case (ModelComponentId.Pure(ic @ IndexedComponentId.Pure(_)), fcRc) => ic -> fcRc
    }

    val rcPureIndexedIds: List[(IndexedComponentId.Indexed, (FormComponent, RevealingChoice))] =
      rcModelComponentIds.collect {
        case (ModelComponentId.Pure(ic @ IndexedComponentId.Indexed(_, _)), fcRc) => ic -> fcRc
      }

    val rcPureIndexedIdsMap
      : Map[BaseComponentId, List[(IndexedComponentId.Indexed, (FormComponent, RevealingChoice))]] =
      rcPureIndexedIds.groupBy(_._1.baseComponentId)

    val purePureFields: F[List[Field]] =
      processRevealingChoices(rcPurePureIds.map(_._2), false)((a, b) => Field(FieldName(a.id.value), b))

    val pureIndexedFields: F[List[Field]] = rcPureIndexedIdsMap.toList.traverse {
      case (baseComponentId, xs) =>
        val objectStructuresF: F[List[ObjectStructure]] = processRevealingChoices(xs.map(_._2), false)((a, b) => b)
        objectStructuresF.map(objectStructures =>
          Field(FieldName(baseComponentId.value), ArrayNode(objectStructures), Map.empty))
    }

    val rcMultiValueIds: List[MultiValueId] = revealingChoices
      .flatMap {
        case (fc, xs) => fc.multiValueId :: xs.options.flatMap(_.revealingFields.map(_.multiValueId))
      }

    ((purePureFields, pureIndexedFields).mapN(_ ++ _), rcMultiValueIds)

  }

  private def processRevealingChoices[A](
    revealingChoices: List[(FormComponent, RevealingChoice)],
    indexedIsPure: Boolean)(f: (FormComponent, ObjectStructure) => A)(implicit l: LangADT): F[List[A]] =
    revealingChoices.traverse {
      case (revealedChoiceFc, revealedChoice) =>
        val selection: Seq[String] =
          formModelVisibilityOptics.data.many(revealedChoiceFc.modelComponentId).getOrElse(Seq.empty)
        val fieldsF: F[List[Field]] =
          buildMultiField(revealedChoice.options.flatMap(_.revealingFields.map(_.multiValueId)).reverse, indexedIsPure)
        fieldsF.map { field =>
          val choiceField =
            if (revealedChoice.multiValue) {
              Field(FieldName("choices"), ArrayNode(selection.map(TextNode).toList))
            } else {
              Field(FieldName("choice"), TextNode(selection.headOption.getOrElse("")))
            }
          val objectStructure = ObjectStructure(
            List(
              choiceField,
              Field(FieldName("revealed"), ObjectStructure(field))
            )
          )
          f(revealedChoiceFc, objectStructure)
        }
    }

  private def processPurePure(
    xs: List[(ModelComponentId.Pure, IndexedComponentId.Pure)]
  )(
    implicit l: LangADT
  ): F[List[Field]] =
    xs.traverse {
        case (modelComponentId, pure) =>
          if (isMultiSelectionIds(modelComponentId)) {
            formModelVisibilityOptics.data
              .many(modelComponentId)
              .map { answers =>
                val arrayNode = ArrayNode(answers.map(_.trim).filterNot(_.isEmpty).map(TextNode).toList)
                Field(FieldName(pure.baseComponentId.value), arrayNode, Map.empty)
              }
              .pure[F]
          } else {
            formModelVisibilityOptics.data
              .one(modelComponentId)
              .traverse { value =>
                valueForFieldType(modelComponentId, value).map(answer =>
                  Field(FieldName(pure.baseComponentId.value), TextNode(answer), Map.empty))

              }
          }
      }
      .map(_.collect {
        case Some(x) => x
      })

  private def processPureIndexed(
    xs: Map[BaseComponentId, List[(ModelComponentId.Pure, IndexedComponentId.Indexed)]],
    indexedIsPure: Boolean
  )(
    implicit l: LangADT
  ): F[List[Field]] =
    xs.toList
      .traverse {
        case (baseComponentId, xss) =>
          val textNodesF: F[List[StructuredFormValue]] = xss
            .traverse {
              case (modelComponentId, indexed) =>
                if (isMultiSelectionIds(modelComponentId)) {
                  formModelVisibilityOptics.data
                    .many(modelComponentId)
                    .map(xs => ArrayNode(xs.map(TextNode).toList): StructuredFormValue)
                    .pure[F]
                } else {
                  formModelVisibilityOptics.data
                    .one(modelComponentId)
                    .traverse { value =>
                      valueForFieldType(modelComponentId, value).map(answer => TextNode(answer): StructuredFormValue)
                    }
                }
            }
            .map(_.collect {
              case Some(x) => x
            })

          for {
            textNodes <- textNodesF
          } yield {
            if (indexedIsPure) {
              textNodes.headOption.map(textNode => Field(FieldName(baseComponentId.value), textNode, Map.empty))
            } else {
              Some(Field(FieldName(baseComponentId.value), ArrayNode(textNodes), Map.empty))
            }
          }

      }
      .map(_.collect {
        case Some(x) => x
      })

  private def alternativeNames(
    modelComponentId: ModelComponentId.Pure,
    atom: Atom): Map[StructuredFormDataFieldNamePurpose, FieldName] =
    formModelVisibilityOptics.formModel.fcLookup.get(modelComponentId.toFormComponentId) match {
      case Some(IsMultiField(mf)) => mf.alternateNamesFor(atom)
      case _                      => Map.empty[StructuredFormDataFieldNamePurpose, FieldName]
    }

  private def processMultiPure(
    xs: List[(IndexedComponentId.Pure, ModelComponentId.Pure, NonEmptyList[ModelComponentId.Atomic])]): List[Field] =
    xs.map {
      case (indexedComponentId, modelComponentId, atomics) =>
        val atoms: List[Field] = atomics.toList
          .map { atomic =>
            val atom = atomic.atom
            val alternatives = alternativeNames(modelComponentId, atom)

            formModelVisibilityOptics.data
              .one(atomic)
              .map(answer => Field(FieldName(atom.value), TextNode(answer), alternatives))
          }
          .collect {
            case Some(x) => x
          }
        Field(FieldName(indexedComponentId.baseComponentId.value), ObjectStructure(atoms), Map.empty)
    }

  private def processMultiIndexed(
    multiIndexedMap: Map[
      BaseComponentId,
      List[(IndexedComponentId.Indexed, ModelComponentId.Pure, NonEmptyList[ModelComponentId.Atomic])]],
    indexedIsPure: Boolean): List[Field] =
    multiIndexedMap.toList
      .map {
        case (baseComponentId, xs) =>
          val structures: List[ObjectStructure] = xs.map {
            case (indexedComponentId, modelComponentId, atomics) =>
              val atoms: List[Field] = atomics.toList
                .map { atomic =>
                  val atom = atomic.atom
                  val alternatives = alternativeNames(modelComponentId, atom)

                  formModelVisibilityOptics.data
                    .one(atomic)
                    .map(answer => Field(FieldName(atom.value), TextNode(answer), alternatives))
                }
                .collect {
                  case Some(x) => x
                }
              ObjectStructure(atoms)

          }
          if (indexedIsPure) {
            structures.headOption.map(textNode => Field(FieldName(baseComponentId.value), textNode, Map.empty))
          } else {
            Some(Field(FieldName(baseComponentId.value), ArrayNode(structures), Map.empty))
          }
      }
      .collect {
        case Some(x) => x
      }

  private def buildMultiField(multiValueIds: List[MultiValueId], indexedIsPure: Boolean)(
    implicit l: LangADT): F[List[Field]] = {

    val multiValuePures: List[MultiValueId.Pure] = multiValueIds.collect {
      case x: MultiValueId.Pure => x
    }

    val multiValues: List[MultiValueId.MultiValue] = multiValueIds.collect {
      case x: MultiValueId.MultiValue => x
    }

    val purePure: List[(ModelComponentId.Pure, IndexedComponentId.Pure)] = multiValuePures.collect {
      case MultiValueId.Pure(mc @ ModelComponentId.Pure(p @ IndexedComponentId.Pure(_))) => mc -> p
    }

    val pppF = processPurePure(purePure)

    val pureIndexed: List[(ModelComponentId.Pure, IndexedComponentId.Indexed)] = multiValuePures.collect {
      case MultiValueId.Pure(mc @ ModelComponentId.Pure(i @ IndexedComponentId.Indexed(_, _))) => mc -> i
    }

    val pureIndexedMap: Map[BaseComponentId, List[(ModelComponentId.Pure, IndexedComponentId.Indexed)]] =
      pureIndexed.groupBy(_._1.baseComponentId).toMap

    val ppiF = processPureIndexed(pureIndexedMap, indexedIsPure)

    val multiPure: List[(IndexedComponentId.Pure, ModelComponentId.Pure, NonEmptyList[ModelComponentId.Atomic])] =
      multiValues.collect {
        case MultiValueId.MultiValue(mc @ ModelComponentId.Pure(p @ IndexedComponentId.Pure(_)), atoms) =>
          (p, mc, atoms)
      }

    val pmp = processMultiPure(multiPure)

    val multiIndexed: List[(IndexedComponentId.Indexed, ModelComponentId.Pure, NonEmptyList[ModelComponentId.Atomic])] =
      multiValues.collect {
        case MultiValueId.MultiValue(mc @ ModelComponentId.Pure(i @ IndexedComponentId.Indexed(_, _)), atoms) =>
          (i, mc, atoms)
      }

    val multiIndexedMap: Map[
      BaseComponentId,
      List[(IndexedComponentId.Indexed, ModelComponentId.Pure, NonEmptyList[ModelComponentId.Atomic])]] =
      multiIndexed.groupBy(_._1.baseComponentId).toMap

    val pmi = processMultiIndexed(multiIndexedMap, indexedIsPure)

    for {
      ppp <- pppF
      ppi <- ppiF
    } yield ppp ++ ppi ++ pmp ++ pmi

  }

  private def lookupIdFromLabel(label: LookupLabel, register: Register)(implicit l: LangADT): F[String] =
    lookupRegistry
      .get(register)
      .flatMap {
        case RadioLookup(options)      => options.lookupInfo(label)
        case AjaxLookup(options, _, _) => options.lookupInfo(label)
      } match {
      case Some(LookupInfo(LookupId(id), _)) => id.pure[F]
      case None =>
        me.raiseError(StructuredFormDataBuilderException(s"Cannot find '${label.label}' in register $register"))
    }

  private def valueForFieldType(
    modelComponentId: ModelComponentId.Pure,
    value: String
  )(
    implicit l: LangADT
  ): F[String] =
    formModelVisibilityOptics.formModel.fcLookup.get(modelComponentId.toFormComponentId) match {
      case Some(IsText(Text(Lookup(register), _, _, _))) => lookupIdFromLabel(LookupLabel(value), register)
      case None =>
        me.raiseError(
          StructuredFormDataBuilderException(
            s"Cannot find component ${modelComponentId.toFormComponentId} in form model."))
      case _ => value.pure[F]
    }
}

case class StructuredFormDataBuilderException(message: String) extends Exception
