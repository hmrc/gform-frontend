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

package uk.gov.hmrc.gform.keystore

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.commons.BigDecimalUtil.toBigDecimalDefault
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.models.helpers.RepeatFormComponentIds
import uk.gov.hmrc.gform.sharedmodel.LabelHelper
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{ Success, Try }

object RepeatingComponentService {

  def getRepeatFormComponentIds(fcs: List[FormComponent]): RepeatFormComponentIds =
    RepeatFormComponentIds(fcId => fcs.filter(_.id.value.endsWith(fcId.value)).map(_.id))

  def sumFunctionality(
    field: FormCtx,
    formTemplate: FormTemplate,
    data: Map[FormComponentId, Seq[String]]): BigDecimal = {
    val atomicFields: List[FormComponent] = formTemplate.sections.flatMap(_.fields)
    val repeatFormComponentIds = getRepeatFormComponentIds(formTemplate.expandFormTemplate.allFCs)
    val fcIds: List[FormComponentId] = repeatFormComponentIds.op(FormComponentId(field.value))
    fcIds.map(id => data.get(id).flatMap(_.headOption).fold(0: BigDecimal)(toBigDecimalDefault)).sum
  }

  def getAllSections(formTemplate: FormTemplate, data: Map[FormComponentId, Seq[String]]): List[Section] =
    formTemplate.sections
      .flatMap { section =>
        if (isRepeatingSection(section)) {
          generateDynamicSections(section, formTemplate, data)
        } else {
          List(section)
        }
      }

  private def isRepeatingSection(section: Section) = section.repeatsMax.isDefined && section.repeatsMin.isDefined

  private def generateDynamicSections(
    section: Section,
    formTemplate: FormTemplate,
    data: Map[FormComponentId, Seq[String]]): List[Section] = {

    val count = getRequestedCount(section.repeatsMax.get, formTemplate, data)

    (1 to count).map { i =>
      copySection(section, i, data)
    }.toList

  }

  private def copySection(section: Section, index: Int, data: Map[FormComponentId, Seq[String]]) = {
    def copyField(field: FormComponent): FormComponent =
      field.`type` match {
        case grp @ Group(fields, _, _, _, _, _) =>
          field.copy(
            id = FormComponentId(s"${index}_${field.id.value}"),
            `type` = grp.copy(fields = fields.map(copyField))
          )
        case _ =>
          field.copy(
            id = FormComponentId(s"${index}_${field.id.value}")
          )
      }

    section.copy(
      title = buildText(Some(section.title), index, data).getOrElse(""),
      shortName = buildText(section.shortName, index, data),
      fields = section.fields.map(copyField)
    )
  }

  private def buildText(
    template: Option[String],
    index: Int,
    data: Map[FormComponentId, Seq[String]]): Option[String] = {

    def evaluateTextExpression(str: String) = {
      val field = str.replaceFirst("""\$\{""", "").replaceFirst("""\}""", "")
      if (field.startsWith("n_")) {
        if (index == 1) {
          val fieldName = field.replaceFirst("n_", "")
          data.getOrElse(FormComponentId(fieldName), Seq("")).mkString
        } else {
          val fieldName = field.replaceFirst("n_", s"${index - 1}_")
          data.getOrElse(FormComponentId(fieldName), Seq("")).mkString
        }
      } else {
        data.getOrElse(FormComponentId(field), Seq("")).mkString
      }
    }

    def getEvaluatedText(str: String) = {
      val pattern = """.*(\$\{.*\}).*""".r
      val expression = str match {
        case pattern(txtExpr) => txtExpr
        case _                => ""
      }
      val evaluatedText = evaluateTextExpression(expression)
      str.replace(expression, evaluatedText)
    }

    template match {
      case Some(inputText) => Some(getEvaluatedText(inputText).replace("$n", index.toString))
      case _               => None
    }
  }

  //This Evaluation is for the repeating sections, this will not become values.
  private def evaluateExpression(
    expr: Expr,
    formTemplate: FormTemplate,
    data: Map[FormComponentId, Seq[String]]): Int = {
    def eval(expr: Expr): Int = expr match {
      case Add(expr1, expr2)         => eval(expr1) + eval(expr2)
      case Multiply(expr1, expr2)    => eval(expr1) * eval(expr2)
      case Subtraction(expr1, expr2) => eval(expr1) - eval(expr2)
      case Sum(ctx @ FormCtx(_))     => RepeatingComponentService.sumFunctionality(ctx, formTemplate, data).toInt
      case formExpr @ FormCtx(_)     => getFormFieldIntValue(TextExpression(formExpr), data)
      case Constant(value)           => Try(value.toInt).toOption.getOrElse(0)
      // case AuthCtx(value: AuthInfo) =>
      // case EeittCtx(value: Eeitt) =>
      case _ => 0
    }
    eval(expr)
  }

  /**
    * This method decide if section is expanded based on repeated group or simple numeric expression
   **/
  private def getRequestedCount(
    expr: TextExpression,
    formTemplate: FormTemplate,
    data: Map[FormComponentId, Seq[String]]): Int = {

    val repeatingGroupsFound = findRepeatingGroupsContainingField(expr, formTemplate)

    if (repeatingGroupsFound.isEmpty) {
      evaluateExpression(expr.expr, formTemplate, data)
    } else {
      val groupFieldValue: FormComponent = repeatingGroupsFound.head

      groupFieldValue match {
        case IsGroup(group) =>
          val groups: List[GroupList] = getAllFieldsInGroup(groupFieldValue, group, data)
          groups.map(_.componentList.size).sum
        case _ => 0
      }
    }
  }

  private def getFormFieldIntValue(expr: TextExpression, data: Map[FormComponentId, Seq[String]]): Int = {

    val id = extractFieldId(expr)

    data.get(FormComponentId(id)) match {
      case Some(value) => Try(value.head.toInt).toOption.getOrElse(0)
      case None        => 0
    }
  }

  private def extractFieldId(expr: TextExpression) =
    expr.expr match {
      case FormCtx(fieldId) => fieldId
      case _                => ""
    }

  private def findRepeatingGroupsContainingField(
    expr: TextExpression,
    formTemplate: FormTemplate): Set[FormComponent] = {

    val id = extractFieldId(expr)

    def findRepeatingGroups(groupField: Option[FormComponent], fieldList: List[FormComponent]): Set[FormComponent] =
      fieldList.flatMap { field =>
        field.`type` match {
          case Group(fields, _, repMax, _, _, _) if repMax.isDefined          => findRepeatingGroups(Some(field), fields)
          case othertype if groupField.isDefined && field.id.value.equals(id) => List(groupField.get)
          case _                                                              => Nil
        }
      }.toSet

    formTemplate.sections.flatMap(section => findRepeatingGroups(None, section.fields)).toSet
  }

  def atomicFields(section: BaseSection): List[FormComponent] = {
    def loop(fields: List[FormComponent]): List[FormComponent] =
      fields
        .flatMap { fv =>
          fv.`type` match {
            case groupField @ Group(_, _, _, _, _, _) =>
              section match {
                case s @ Section(_, _, _, _, _, _, _, _, _, _) =>
                  s.expandSection.expandedFCs.flatMap(_.expandedFC)
                case DeclarationSection(_, _, _, _) => loop(groupField.fields)
                case _                              => List.empty
              }
            case _ => List(fv)
          }
        }

    loop(section.fields)
  }
}
