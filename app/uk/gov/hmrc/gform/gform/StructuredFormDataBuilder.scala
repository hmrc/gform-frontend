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

package uk.gov.hmrc.gform.gform

import cats.{ Monad, MonadError }
import cats.data.NonEmptyList
import cats.implicits._
import play.api.i18n.Messages

import scala.util.Try
import uk.gov.hmrc.auth.core.retrieve.ItmpAddress
import uk.gov.hmrc.gform.auth.models.ItmpRetrievals
import uk.gov.hmrc.gform.commons.BigDecimalUtil.toBigDecimalSafe
import uk.gov.hmrc.gform.eval.ExpressionResult.DateResult
import uk.gov.hmrc.gform.eval.ExpressionResultWithTypeInfo
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.models.{ Atom, Bracket, Visibility }
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId, MultiValueId }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ RetrieveDataType, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.{ DestinationList, DestinationPrint }
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ Field, FieldName, StructuredFormDataFieldNamePurpose, StructuredFormValue }
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue.{ ArrayNode, ObjectStructure, TextNode }
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions
import uk.gov.hmrc.gform.ops.FormComponentOps

object StructuredFormDataBuilder {
  def apply[D <: DataOrigin, F[_]: Monad](
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    destinations: Destinations,
    expressionsOutput: Option[ExpressionOutput],
    lookupRegistry: LookupRegistry
  )(implicit l: LangADT, m: Messages, me: MonadError[F, Throwable]): F[ObjectStructure] = {

    def zeroPadding(x: Int) =
      "%02d".format(x)

    def mkDate(expressionId: ExpressionId, day: Int, month: Int, year: Int) =
      Field(
        FieldName(expressionId.id),
        StructuredFormValue.ObjectStructure(
          List(
            Field(FieldName("day"), StructuredFormValue.TextNode(zeroPadding(day))),
            Field(FieldName("month"), StructuredFormValue.TextNode(zeroPadding(month))),
            Field(FieldName("year"), StructuredFormValue.TextNode(year.toString))
          )
        )
      )

    val expressionsOutputFields = expressionsOutput.fold(List.empty[Field]) { eo =>
      eo.lookup.toList.flatMap { case (expressionId, expr) =>
        expr match {
          case AuthCtx(AuthInfo.ItmpAddress) =>
            val maybeItmpRetrievals: Option[ItmpRetrievals] =
              formModelVisibilityOptics.recalculationResult.evaluationContext.thirdPartyData.itmpRetrievals

            val maybeItmpAddress: Option[ItmpAddress] = maybeItmpRetrievals.flatMap(_.itmpAddress)

            maybeItmpAddress.map { itmpAddress =>
              val joinLines45 = itmpAddress.line4.map(_ + " ") |+| itmpAddress.line5
              val fields = List(
                "line1"    -> itmpAddress.line1,
                "line2"    -> itmpAddress.line2,
                "line3"    -> itmpAddress.line3,
                "line4"    -> joinLines45,
                "postCode" -> itmpAddress.postCode,
                "country"  -> itmpAddress.countryName
              ).collect { case (key, Some(value)) =>
                (key, value)
              }.map { case (key, value) =>
                Field(FieldName(key), StructuredFormValue.TextNode(value))
              }

              Field(
                FieldName(expressionId.id),
                StructuredFormValue.ObjectStructure(
                  fields
                )
              )
            }.toList

          case AuthCtx(AuthInfo.ItmpDateOfBirth) =>
            val maybeItmpRetrievals: Option[ItmpRetrievals] =
              formModelVisibilityOptics.recalculationResult.evaluationContext.thirdPartyData.itmpRetrievals
            maybeItmpRetrievals
              .flatMap(_.itmpDateOfBirth)
              .map(DateHelperFunctions.convertToDateExpr)
              .map { dateExpr =>
                mkDate(expressionId, dateExpr.day, dateExpr.month, dateExpr.year)
              }
              .toList

          case otherwise =>
            val expressionResultWithInfo: ExpressionResultWithTypeInfo =
              formModelVisibilityOptics.evalAndApplyTypeInfoFirst(expr)

            expressionResultWithInfo.expressionResult match {
              case DateResult(dateExpr) =>
                List(
                  mkDate(expressionId, dateExpr.getDayOfMonth, dateExpr.getMonthValue, dateExpr.getYear)
                )
              case _ =>
                val result = expressionResultWithInfo.stringRepresentation
                if (result.trim.isEmpty) {
                  Nil
                } else {
                  List(Field(FieldName(expressionId.id), StructuredFormValue.TextNode(result)))
                }
            }
        }
      }
    }
    new StructuredFormDataBuilder[D, F](
      formModelVisibilityOptics,
      destinations,
      expressionsOutputFields,
      lookupRegistry
    )
      .build()
      .map(StructuredFormValue.ObjectStructure.apply)
  }
}

class StructuredFormDataBuilder[D <: DataOrigin, F[_]: Monad](
  formModelVisibilityOptics: FormModelVisibilityOptics[D],
  destinations: Destinations,
  expressionsOutputFields: List[Field],
  lookupRegistry: LookupRegistry
)(implicit me: MonadError[F, Throwable]) {

  private val isMultiSelectionIds: Set[ModelComponentId] = formModelVisibilityOptics.formModel.allMultiSelectionIds

  private val isStrictlyMultiSelectionIds: Set[ModelComponentId] = formModelVisibilityOptics.formModel.allFormComponents
    .collect {
      case fc @ IsChoice(Choice(Checkbox, _, _, _, _, _, _, _, _, _, _, _)) => fc.id
      case fc @ IsRevealingChoice(RevealingChoice(_, true))                 => fc.id
    }
    .map(_.modelComponentId)
    .toSet

  private val sanitiseRequiredIds: Set[BaseComponentId] = formModelVisibilityOptics.formModel.allFormComponents
    .collect {
      case fc if fc.isSterling || fc.isPositiveNumber || fc.isNumber => fc.id
    }
    .map(_.baseComponentId)
    .toSet

  private val choicesWithDynamic: List[
    (ModelComponentId, Either[NonEmptyList[(Int, OptionData.ValueBased)], NonEmptyList[(Int, OptionData.IndexBased)]])
  ] =
    formModelVisibilityOptics.formModel.allFormComponents.collect {
      case fc @ IsChoice(Choice(_, options, _, _, _, _, _, _, _, _, _, _)) if options.exists(_.dynamic.isDefined) =>
        val indexBased: List[OptionData.IndexBased] = options.collect { case ib: OptionData.IndexBased =>
          ib
        }

        val maybeIndexBasedNel: Option[NonEmptyList[(Int, OptionData.IndexBased)]] =
          NonEmptyList
            .fromList(indexBased)
            .map { indexBasedNel =>
              indexBasedNel.map { optionData =>
                val default = (0, optionData)
                optionData.dynamic.fold(default) {
                  case Dynamic.DataRetrieveBased(IndexOfDataRetrieveCtx(_, index)) => index -> optionData
                  case Dynamic.ATLBased(formComponentId) =>
                    formComponentId.modelComponentId.maybeIndex.fold(default)(_ -> optionData)
                }
              }
            }

        val valueBased: List[OptionData.ValueBased] = options.collect { case vb: OptionData.ValueBased =>
          vb
        }

        val maybeValueBasedNel: Option[NonEmptyList[(Int, OptionData.ValueBased)]] =
          NonEmptyList.fromList(valueBased).map { valueBasedNel =>
            valueBasedNel.map { optionData =>
              val default = (0, optionData)
              optionData.dynamic.fold(default) {
                case Dynamic.DataRetrieveBased(IndexOfDataRetrieveCtx(_, index)) => index -> optionData
                case Dynamic.ATLBased(formComponentId) =>
                  formComponentId.modelComponentId.maybeIndex.fold(default)(_ -> optionData)
              }
            }
          }

        val res: Either[NonEmptyList[(Int, OptionData.ValueBased)], NonEmptyList[(Int, OptionData.IndexBased)]] =
          (maybeIndexBasedNel, maybeValueBasedNel) match {
            case (None, Some(valueBasedNel)) => Left(valueBasedNel)
            case (Some(indexBasedNel), None) => Right(indexBasedNel)
            case (None, None)                => throw new Exception(s"No options found for component ${fc.id}. $fc")
            case (Some(_), Some(_)) =>
              throw new Exception(s"Component ${fc.id} has mix of index based and value based options. $fc")
          }

        fc.id.modelComponentId -> res
    }

  private val ignoredComponentIds: Set[ModelComponentId] = formModelVisibilityOptics.formModel.allFormComponents
    .collect {
      case fc @ IsGroup(_)              => fc.id
      case fc @ IsInformationMessage(_) => fc.id
      case fc @ IsMiniSummaryList(_)    => fc.id
      case fc @ IsTableComp(_)          => fc.id
    }
    .map(_.modelComponentId)
    .toSet

  def build()(implicit l: LangADT, m: Messages): F[List[Field]] =
    destinations match {
      case DestinationList(_, _, _)  => buildSections
      case DestinationPrint(_, _, _) => List.empty[Field].pure[F]
    }

  private def buildSections(implicit l: LangADT, m: Messages): F[List[Field]] = {

    val postcodeLookupsIds: Set[BaseComponentId] = formModelVisibilityOptics.formModel.allFormComponents.collect {
      case fc @ IsPostcodeLookup(_) =>
        fc.id.baseComponentId
    }.toSet

    /*
     * What follows is a mess. Implementation was written before Bracket model
     * was introduced.
     *
     * New implementation should be based on iteration over brackets, like so:
     *
     *  formModelVisibilityOptics.formModel.brackets.brackets.map { bracket =>
     *    bracket.fold { nonRepeatingPageBracket =>
     *      ???
     *    } { repeatingPageBracket =>
     *      ???
     *    } { addToListBracket =>
     *      ???
     *    }
     *  }
     */
    val (addToListFields, addToListMultiValueIds) = buildAddToList(postcodeLookupsIds)
    val (revealingChoiceFields, rcMultiValueIds) = buildRevealingChoice

    val addToListAndRcMultivalues = addToListMultiValueIds ++ rcMultiValueIds

    val allMultiValueIds: List[MultiValueId] = formModelVisibilityOptics.formModel.allMultiValueIds

    val multiValuesNotProcessedYet: List[MultiValueId] =
      allMultiValueIds
        .filterNot(addToListAndRcMultivalues.contains)
        .filterNot(multiValueId => ignoredComponentIds(multiValueId.modelComponentId))

    val restOfTheFields: F[List[Field]] = buildMultiField(multiValuesNotProcessedYet, false, postcodeLookupsIds)

    val fields =
      (addToListFields, revealingChoiceFields, restOfTheFields, expressionsOutputFields.pure[F]).mapN(_ ++ _ ++ _ ++ _)

    fields.map(_.map(field => field.copy(value = sanitiseStructuredFormValue(field))))
  }

  private def sanitiseStructuredFormValue(field: Field): StructuredFormValue =
    field.value match {
      case ObjectStructure(fields) =>
        ObjectStructure(fields.map(field => field.copy(value = sanitiseStructuredFormValue(field))))
      case t @ TextNode(_) =>
        if (t.value.nonEmpty && sanitiseRequiredIds(FormComponentId(field.name.name).baseComponentId)) {
          val cleanedValue = t.value.replace("£", "")
          toBigDecimalSafe(cleanedValue)
            .map(value => TextNode(value.toString))
            .getOrElse(throw new NumberFormatException(s"Unable to convert value '${t.value}' to a number."))
        } else {
          t
        }
      case ArrayNode(elements) =>
        ArrayNode(elements.map(element => sanitiseStructuredFormValue(field.copy(value = element))))
    }

  private def buildAddToList(
    postcodeLookupsIds: Set[BaseComponentId]
  )(implicit l: LangADT, m: Messages): (F[List[Field]], List[MultiValueId]) = {
    val addToLists: List[(AddToListId, List[MultiValueId])] =
      formModelVisibilityOptics.formModel.addToListBrackets
        .flatMap(bracket =>
          bracket.toPageModel.toList.map(
            _.fold(singleton => bracket.source.id -> singleton.allMultiValueIds)(_ =>
              bracket.source.id -> List.empty[MultiValueId]
            )(repeater => bracket.source.id -> repeater.allMultiValueIds)
          )
        )

    val addToListMultiValueIds: List[MultiValueId] = addToLists.flatMap(_._2)

    val addToListsMap: Map[AddToListId, List[MultiValueId]] =
      addToLists.groupBy(_._1).view.mapValues(_.map(_._2)).mapValues(_.flatten).toMap

    val multiValueIdByIndex: Map[AddToListId, Map[Int, List[MultiValueId]]] =
      addToListsMap.view.mapValues(_.groupBy(_.modelComponentId.indexedComponentId.maybeIndex.getOrElse(0))).toMap

    val addToListFields: F[List[Field]] = {
      val allRevealingChoicesIds: List[(FormComponent, RevealingChoice)] =
        formModelVisibilityOptics.formModel.allFormComponents.collect { case fc @ IsRevealingChoice(revealingChoice) =>
          fc -> revealingChoice
        }

      val rcModelComponentIds: List[(ModelComponentId, (FormComponent, RevealingChoice))] = allRevealingChoicesIds.map {
        case (fc, rc) => (fc.modelComponentId, (fc, rc))
      }

      multiValueIdByIndex.toList.traverse { case (addToListId, indexWithMultiValues) =>
        val objectStructuresF: F[List[ObjectStructure]] =
          indexWithMultiValues.toList.sortBy { case (index, _) => index }.traverse { case (_, multiValues) =>
            val modelComponentIds: Set[ModelComponentId] = multiValues.map(_.modelComponentId).toSet

            val addToListRevealingChoices: List[(ModelComponentId, (FormComponent, RevealingChoice))] =
              rcModelComponentIds.filter { case (modelComponentId, _) =>
                modelComponentIds(modelComponentId)
              }

            val rcPureIndexedIds: List[(IndexedComponentId.Indexed, (FormComponent, RevealingChoice))] =
              addToListRevealingChoices.collect {
                case (ModelComponentId.Pure(ic @ IndexedComponentId.Indexed(_, _)), fcRc) => ic -> fcRc
              }

            val rcPureIndexedIdsMap
              : Map[BaseComponentId, List[(IndexedComponentId.Indexed, (FormComponent, RevealingChoice))]] =
              rcPureIndexedIds.groupBy(_._1.baseComponentId)

            val revealingChoiceFields: F[List[Field]] = rcPureIndexedIdsMap.toList
              .traverse { case (baseComponentId, xs) =>
                val revealingChoices: List[(FormComponent, RevealingChoice)] = xs.map(_._2)
                val objectStructuresF: F[List[ObjectStructure]] =
                  processRevealingChoices(revealingChoices, true)((a, b) => b)
                objectStructuresF.map(
                  _.headOption
                    .map(objectStructure => Field(FieldName(baseComponentId.value), objectStructure, Map.empty))
                )
              }
              .map(_.flatten)

            val rcIndexedComponents: List[IndexedComponentId.Indexed] = rcPureIndexedIds.map(_._1)

            val rcComponentsMultiValues: List[MultiValueId] =
              rcPureIndexedIds
                .map(_._2._2)
                .flatMap { revealedChoice =>
                  revealedChoice.options.flatMap(_.revealingFields).map(_.multiValueId)
                }

            val multiValuesNoRc: List[MultiValueId] = multiValues
              .filterNot { multiValue =>
                rcIndexedComponents.contains(multiValue.modelComponentId.indexedComponentId) ||
                rcComponentsMultiValues.contains(multiValue)
              }
              .filterNot(multiValue => ignoredComponentIds(multiValue.modelComponentId))

            val restOfTheFields: F[List[Field]] = buildMultiField(multiValuesNoRc, true, postcodeLookupsIds)

            (restOfTheFields, revealingChoiceFields).mapN(_ ++ _).map(ObjectStructure(_))
          }

        objectStructuresF.map(objectStructures =>
          Field(FieldName(addToListId.formComponentId.value), ArrayNode(objectStructures), Map.empty)
        )

      }
    }

    (addToListFields, addToListMultiValueIds)

  }

  private def buildRevealingChoice(implicit l: LangADT, m: Messages): (F[List[Field]], List[MultiValueId]) = {

    val revealingChoices: List[(FormComponent, RevealingChoice)] =
      (formModelVisibilityOptics.formModel.nonRepeatingPageBrackets ++ formModelVisibilityOptics.formModel.repeatingPageBrackets)
        .flatMap(_.toPageModel.toList)
        .flatMap(_.allFormComponents)
        .collect { case fc @ IsRevealingChoice(revealingChoice) =>
          fc -> revealingChoice
        }

    val rcModelComponentIds: List[(ModelComponentId, (FormComponent, RevealingChoice))] = revealingChoices.map {
      case (fc, rc) => (fc.modelComponentId, (fc, rc))
    }

    val rcPurePureIds: List[(IndexedComponentId.Pure, (FormComponent, RevealingChoice))] = rcModelComponentIds.collect {
      case (ModelComponentId.Pure(ic @ IndexedComponentId.Pure(_)), fcRc) => ic -> fcRc
    }

    val rcPureIndexedIds: List[(IndexedComponentId.Indexed, (FormComponent, RevealingChoice))] =
      rcModelComponentIds.collect { case (ModelComponentId.Pure(ic @ IndexedComponentId.Indexed(_, _)), fcRc) =>
        ic -> fcRc
      }

    val rcPureIndexedIdsMap
      : Map[BaseComponentId, List[(IndexedComponentId.Indexed, (FormComponent, RevealingChoice))]] =
      rcPureIndexedIds.groupBy(_._1.baseComponentId)

    val purePureFields: F[List[Field]] =
      processRevealingChoices(rcPurePureIds.map(_._2), true)((a, b) => Field(FieldName(a.id.value), b))

    val pureIndexedFields: F[List[Field]] = rcPureIndexedIdsMap.toList.traverse { case (baseComponentId, xs) =>
      val objectStructuresF: F[List[ObjectStructure]] = processRevealingChoices(xs.map(_._2), true)((a, b) => b)
      objectStructuresF.map(objectStructures =>
        Field(FieldName(baseComponentId.value), ArrayNode(objectStructures), Map.empty)
      )
    }

    val rcMultiValueIds: List[MultiValueId] = revealingChoices
      .flatMap { case (fc, xs) =>
        fc.multiValueId :: xs.options.flatMap(_.revealingFields.map(_.multiValueId))
      }

    ((purePureFields, pureIndexedFields).mapN(_ ++ _), rcMultiValueIds)

  }

  private def sortMany(xs: Seq[String]): Seq[String] =
    xs.sortBy(s => Try(s.toInt).toOption.getOrElse(0))

  private def processRevealingChoices[A](
    revealingChoices: List[(FormComponent, RevealingChoice)],
    indexedIsPure: Boolean
  )(f: (FormComponent, ObjectStructure) => A)(implicit l: LangADT, m: Messages): F[List[A]] =
    revealingChoices.traverse { case (revealedChoiceFc, revealedChoice) =>
      val selection: Seq[String] =
        formModelVisibilityOptics.data
          .many(revealedChoiceFc.modelComponentId)
          .map(sortMany)
          .getOrElse(Seq.empty)
      val fieldsF: F[List[Field]] =
        buildMultiField(
          revealedChoice.options.flatMap(_.revealingFields.map(_.multiValueId)),
          indexedIsPure,
          Set.empty
        )
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

  private def dataRetrieveToObjectStructure(maybeDataRetrieveCtx: (Int, DataRetrieveCtx)): Option[ObjectStructure] = {
    val (selectedIndex, dataRetrieveCtx) = maybeDataRetrieveCtx

    formModelVisibilityOptics.recalculationResult.evaluationContext.thirdPartyData.dataRetrieve
      .flatMap { dataRet =>
        dataRet.get(dataRetrieveCtx.id).map(_.data).flatMap {
          case RetrieveDataType.ListType(data) =>
            data.get(selectedIndex.toLong).map { attributesMap =>
              val fields: List[Field] = attributesMap.toList.map { case (attribute, value) =>
                Field(FieldName(attribute.name), TextNode(value), Map.empty)
              }
              ObjectStructure(fields)
            }
          case RetrieveDataType.ObjectType(_) => None
        }
      }
  }

  private def processPurePure(
    xs: List[(ModelComponentId.Pure, IndexedComponentId.Pure)]
  )(implicit
    l: LangADT,
    m: Messages
  ): F[List[Field]] =
    xs.traverse { case (modelComponentId, pure) =>
      if (isMultiSelectionIds(modelComponentId)) {
        formModelVisibilityOptics.data
          .many(modelComponentId)
          .map(sortMany)
          .map { answers =>
            if (isStrictlyMultiSelectionIds(modelComponentId)) {
              val arrayNode = ArrayNode(answers.map(_.trim).filterNot(_.isEmpty).map(TextNode).toList)
              Field(FieldName(pure.baseComponentId.value), arrayNode, Map.empty)
            } else {
              val fieldValue = evalChoicesWithDynamic(answers, modelComponentId)
              Field(FieldName(pure.baseComponentId.value), fieldValue, Map.empty)
            }
          }
          .pure[F]
      } else {
        formModelVisibilityOptics.data
          .one(modelComponentId)
          .traverse { value =>
            valueForFieldType(modelComponentId, value)
              .map(answer => Field(FieldName(pure.baseComponentId.value), TextNode(answer), Map.empty))
          }
      }
    }.map(_.collect { case Some(x) =>
      x
    })

  private def evalChoicesWithDynamic(
    answers: Seq[String],
    modelComponentId: ModelComponentId.Pure
  )(implicit m: Messages): StructuredFormValue = {
    val answer = answers.headOption.getOrElse("")
    val maybeChoiceWithDynamic: Option[
      (
        ModelComponentId,
        Either[NonEmptyList[(Int, OptionData.ValueBased)], NonEmptyList[(Int, OptionData.IndexBased)]]
      )
    ] =
      choicesWithDynamic.find(_._1 === modelComponentId)

    val default: StructuredFormValue = TextNode(answer)

    maybeChoiceWithDynamic.fold(default) {
      case (_, Left(valueBasedNel)) =>
        val maybeDynamicOptionData: Option[(Int, FormComponentId)] = valueBasedNel.collectFirst {
          case (selectedIndex, OptionData.ValueBased(_, _, _, Some(Dynamic.ATLBased(pointer)), value, _, _))
              if (value match {
                case OptionDataValue.StringBased(v) => v === answer
                case OptionDataValue.ExprBased(expr) =>
                  formModelVisibilityOptics
                    .evalAndApplyTypeInfoFirst(expr)
                    .stringRepresentation === answer
                case _ => false
              }) =>
            selectedIndex -> pointer
        }

        val maybeDataRetrieveCtx: Option[(Int, DataRetrieveCtx)] = valueBasedNel.collectFirst {
          case (
                selectedIndex,
                OptionData.ValueBased(
                  _,
                  _,
                  _,
                  Some(Dynamic.DataRetrieveBased(indexOfDataRetrieveCtx)),
                  value,
                  _,
                  _
                )
              ) if (value match {
                case OptionDataValue.StringBased(v) => v === answer
                case OptionDataValue.ExprBased(expr) =>
                  formModelVisibilityOptics
                    .evalAndApplyTypeInfoFirst(expr)
                    .stringRepresentation === answer
                case _ => false
              }) =>
            selectedIndex -> indexOfDataRetrieveCtx.ctx.copy(id = indexOfDataRetrieveCtx.ctx.id.modelPageId.baseId)
        }

        maybeDynamicOptionData
          .map(createObjectStructureForSelectedIndex)
          .orElse(maybeDataRetrieveCtx.flatMap(dataRetrieveToObjectStructure))
          .getOrElse(default)

      case (_, Right(indexBasedNel)) =>
        val indexAnswer: Int = Try(answer.toInt).toOption.getOrElse(
          throw new Exception(s"Cannot convert answer $answer to an Int")
        )

        val maybeDynamicIndexBased: Option[(Int, FormComponentId)] =
          indexBasedNel.zipWithIndex.collectFirst {
            case (
                  (
                    selectedIndex,
                    OptionData.IndexBased(_, _, _, Some(Dynamic.ATLBased(pointer)), _)
                  ),
                  i
                ) if indexAnswer === i =>
              selectedIndex -> pointer
          }

        val maybeDataRetrieveCtx: Option[(Int, DataRetrieveCtx)] = indexBasedNel.zipWithIndex.collectFirst {
          case (
                (
                  selectedIndex,
                  OptionData.IndexBased(
                    _,
                    _,
                    _,
                    Some(Dynamic.DataRetrieveBased(indexOfDataRetrieveCtx)),
                    _
                  )
                ),
                i
              ) if indexAnswer === i =>
            selectedIndex -> indexOfDataRetrieveCtx.ctx.copy(id = indexOfDataRetrieveCtx.ctx.id.modelPageId.baseId)
        }

        maybeDynamicIndexBased
          .map(createObjectStructureForSelectedIndex)
          .orElse(maybeDataRetrieveCtx.flatMap(dataRetrieveToObjectStructure))
          .getOrElse(default)

    }
  }

  private def createObjectStructureForSelectedIndex(
    data: (Int, FormComponentId)
  ): StructuredFormValue = {
    val (selectedIndex, pointer) = data

    val allAddToListBrackets: List[Bracket.AddToList[Visibility]] =
      formModelVisibilityOptics.formModel.brackets.addToListBrackets

    val maybeAllAddToListBracket: Option[Bracket.AddToList[Visibility]] = allAddToListBrackets.find { bracket =>
      bracket.source.allIds.map(_.baseComponentId).contains(pointer.baseComponentId)
    }

    val maybeIteration: Option[(Bracket.AddToListIteration[Visibility], Int)] =
      maybeAllAddToListBracket.flatMap { bracket =>
        bracket.iterations.zipWithIndex.find { case (bracket, index) =>
          index === selectedIndex - 1
        }
      }

    val modelComponentIdsFromIteration: List[ModelComponentId] = maybeIteration.toList.flatMap { case (iteration, _) =>
      iteration.toPageModel.toList.flatMap(_.allModelComponentIds.toList)
    }

    val answers: List[(BaseComponentId, VariadicValue)] = modelComponentIdsFromIteration.flatMap { modelComponentId =>
      formModelVisibilityOptics.recData.variadicFormData
        .get(modelComponentId)
        .map(modelComponentId.baseComponentId -> _)
    }

    val fields = answers.map { case (baseComponentId, variadicValue) =>
      Field(FieldName(baseComponentId.value), TextNode(variadicValue.toSeq.mkString(",")), Map.empty)
    }

    ObjectStructure(fields)

  }

  private def processPureIndexed(
    xs: List[(BaseComponentId, List[(ModelComponentId.Pure, IndexedComponentId.Indexed)])],
    indexedIsPure: Boolean
  )(implicit
    l: LangADT,
    m: Messages
  ): F[List[Field]] =
    xs.traverse { case (baseComponentId, xss) =>
      val textNodesF: F[List[StructuredFormValue]] = xss
        .traverse { case (modelComponentId, indexed) =>
          if (isMultiSelectionIds(modelComponentId)) {
            formModelVisibilityOptics.data
              .many(modelComponentId)
              .map(sortMany)
              .map { answers =>
                if (isStrictlyMultiSelectionIds(modelComponentId)) {
                  ArrayNode(answers.map(TextNode).toList): StructuredFormValue
                } else {
                  evalChoicesWithDynamic(answers, modelComponentId)
                }
              }
              .pure[F]
          } else {
            formModelVisibilityOptics.data
              .one(modelComponentId)
              .traverse { value =>
                valueForFieldType(modelComponentId, value).map(answer => TextNode(answer): StructuredFormValue)
              }
          }
        }
        .map(_.collect { case Some(x) =>
          x
        })

      for {
        textNodes <- textNodesF
      } yield
        if (indexedIsPure) {
          textNodes.headOption.map(textNode => Field(FieldName(baseComponentId.value), textNode, Map.empty))
        } else {
          Some(Field(FieldName(baseComponentId.value), ArrayNode(textNodes), Map.empty))
        }

    }.map(_.collect { case Some(x) =>
      x
    })

  private def alternativeNames(
    modelComponentId: ModelComponentId.Pure,
    atom: Atom
  ): Map[StructuredFormDataFieldNamePurpose, FieldName] =
    formModelVisibilityOptics.formModel.fcLookup.get(modelComponentId.toFormComponentId) match {
      case Some(IsMultiField(mf)) => mf.alternateNamesFor(atom)
      case _                      => Map.empty[StructuredFormDataFieldNamePurpose, FieldName]
    }

  private def processMultiPure(
    xs: List[(IndexedComponentId.Pure, ModelComponentId.Pure, NonEmptyList[ModelComponentId.Atomic])],
    postcodeLookupsIds: Set[BaseComponentId]
  ): List[Field] =
    xs.map { case (indexedComponentId, modelComponentId, atomics) =>
      val atoms: List[Field] = if (postcodeLookupsIds(indexedComponentId.baseComponentId)) {
        atomsForPostcodeLookup(indexedComponentId)
      } else {
        atomsForModelComponentId(atomics, modelComponentId)
      }

      Field(FieldName(indexedComponentId.baseComponentId.value), ObjectStructure(atoms), Map.empty)
    }

  private def alternativeNamesForPostCodeLookupManualAddress(atom: Atom): String =
    atom match {
      case Atom("street1") => "line1" // Address line 1
      case Atom("street2") => "line2" // Address line 2 (optional)
      case Atom("street3") => "town" // Town or city (optional)
      case Atom(otherwise) => otherwise
    }

  private def atomsForPostcodeLookup(indexedComponentId: IndexedComponentId): List[Field] = {
    val baseFcId = FormComponentId(indexedComponentId.baseComponentId.value)
    val fcId = indexedComponentId.fold(_ => baseFcId)(index => baseFcId.withIndex(index.index))
    val postcodeLookupAddress =
      formModelVisibilityOptics.recalculationResult.evaluationContext.thirdPartyData.addressFor(fcId)
    postcodeLookupAddress match {
      case Some(Left(formData)) =>
        val lookup = formData.toData
        val lines: NonEmptyList[(Atom, String)] =
          Address.summaryPageFields(fcId.modelComponentId.indexedComponentId).map { modelCompoentIdAtomic =>
            modelCompoentIdAtomic.atom -> lookup.getOrElse(modelCompoentIdAtomic, "")
          }
        lines.toList
          .filter(_._2.nonEmpty)
          .map { case (atom, answer) =>
            Field(
              FieldName(alternativeNamesForPostCodeLookupManualAddress(atom)),
              TextNode(answer)
            )
          }

      case Some(Right(addressLookupResult)) =>
        import addressLookupResult.address._
        List(
          "line1"    -> line1,
          "line2"    -> line2,
          "line3"    -> line3,
          "line4"    -> line4,
          "town"     -> town,
          "postcode" -> postcode
        )
          .filter(_._2.nonEmpty)
          .map { case (fieldName, answer) => Field(FieldName(fieldName), TextNode(answer)) }
      case None => List.empty[Field]
    }
  }

  private def atomsForModelComponentId(
    atomics: NonEmptyList[ModelComponentId.Atomic],
    modelComponentId: ModelComponentId.Pure
  ): List[Field] =
    atomics.toList
      .map { atomic =>
        val atom = atomic.atom
        val alternatives = alternativeNames(modelComponentId, atom)

        formModelVisibilityOptics.data
          .one(atomic)
          .map(answer => Field(FieldName(atom.value), TextNode(answer), alternatives))
      }
      .collect { case Some(x) =>
        x
      }

  private def processMultiIndexed(
    multiIndexedMap: Map[BaseComponentId, List[
      (IndexedComponentId.Indexed, ModelComponentId.Pure, NonEmptyList[ModelComponentId.Atomic])
    ]],
    indexedIsPure: Boolean,
    postcodeLookupsIds: Set[BaseComponentId]
  ): List[Field] =
    multiIndexedMap.toList
      .map { case (baseComponentId, xs) =>
        val structures: List[ObjectStructure] = xs.map { case (indexedComponentId, modelComponentId, atomics) =>
          val atoms = if (postcodeLookupsIds(baseComponentId)) {
            atomsForPostcodeLookup(indexedComponentId)
          } else {
            atomsForModelComponentId(atomics, modelComponentId)
          }
          ObjectStructure(atoms)
        }
        if (indexedIsPure) {
          structures.headOption.map(textNode => Field(FieldName(baseComponentId.value), textNode, Map.empty))
        } else {
          Some(Field(FieldName(baseComponentId.value), ArrayNode(structures), Map.empty))
        }
      }
      .collect { case Some(x) =>
        x
      }

  private def buildMultiField(
    multiValueIds: List[MultiValueId],
    indexedIsPure: Boolean,
    postcodeLookupsIds: Set[BaseComponentId]
  )(implicit
    l: LangADT,
    m: Messages
  ): F[List[Field]] = {

    val multiValuePures: List[MultiValueId.Pure] = multiValueIds.collect { case x: MultiValueId.Pure =>
      x
    }

    val multiValues: List[MultiValueId.MultiValue] = multiValueIds.collect { case x: MultiValueId.MultiValue =>
      x
    }

    val purePure: List[(ModelComponentId.Pure, IndexedComponentId.Pure)] = multiValuePures.collect {
      case MultiValueId.Pure(mc @ ModelComponentId.Pure(p @ IndexedComponentId.Pure(_))) => mc -> p
    }

    val pppF = processPurePure(purePure)

    val pureIndexed: List[(ModelComponentId.Pure, IndexedComponentId.Indexed)] = multiValuePures.collect {
      case MultiValueId.Pure(mc @ ModelComponentId.Pure(i @ IndexedComponentId.Indexed(_, _))) => mc -> i
    }

    val pureIndexedSorted: List[(BaseComponentId, List[(ModelComponentId.Pure, IndexedComponentId.Indexed)])] =
      pureIndexed
        .groupBy(_._1.baseComponentId)
        .toList
        .sortBy { case (baseComponentId, _) =>
          pureIndexed.indexWhere(_._1.baseComponentId === baseComponentId)
        }

    val ppiF = processPureIndexed(pureIndexedSorted, indexedIsPure)

    val multiPure: List[(IndexedComponentId.Pure, ModelComponentId.Pure, NonEmptyList[ModelComponentId.Atomic])] =
      multiValues.collect {
        case MultiValueId.MultiValue(mc @ ModelComponentId.Pure(p @ IndexedComponentId.Pure(_)), atoms) =>
          (p, mc, atoms)
      }

    val pmp = processMultiPure(multiPure, postcodeLookupsIds)

    val multiIndexed: List[(IndexedComponentId.Indexed, ModelComponentId.Pure, NonEmptyList[ModelComponentId.Atomic])] =
      multiValues.collect {
        case MultiValueId.MultiValue(mc @ ModelComponentId.Pure(i @ IndexedComponentId.Indexed(_, _)), atoms) =>
          (i, mc, atoms)
      }

    val multiIndexedMap: Map[BaseComponentId, List[
      (IndexedComponentId.Indexed, ModelComponentId.Pure, NonEmptyList[ModelComponentId.Atomic])
    ]] =
      multiIndexed.groupBy(_._1.baseComponentId)

    val pmi = processMultiIndexed(multiIndexedMap, indexedIsPure, postcodeLookupsIds)

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
      case Some(lookupInfo) => lookupInfo.id.id.pure[F]
      case None =>
        me.raiseError(StructuredFormDataBuilderException(s"Cannot find '${label.label}' in register $register"))
    }

  private def valueForFieldType(
    modelComponentId: ModelComponentId.Pure,
    value: String
  )(implicit
    l: LangADT
  ): F[String] =
    formModelVisibilityOptics.formModel.fcLookup.get(modelComponentId.toFormComponentId) match {
      case Some(IsText(Text(Lookup(register, _), _, _, _, _, _, _))) => lookupIdFromLabel(LookupLabel(value), register)
      case None =>
        me.raiseError(
          StructuredFormDataBuilderException(
            s"Cannot find component ${modelComponentId.toFormComponentId} in form model."
          )
        )
      case _ => value.pure[F]
    }
}

case class StructuredFormDataBuilderException(message: String) extends Exception
