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

package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json.{ Format, Json }
import uk.gov.hmrc.gform.models.ExpandUtils
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BooleanExpr, Concat, Constant, Expr, FormComponentId }
import uk.gov.hmrc.gform.gform.{ SmartStringIfReplaceSubstituter, SmartStringIfReplaceSubstitutions }
import uk.gov.hmrc.gform.gform.{ SmartStringIfBranchSubstituter, SmartStringIfBranchSubstitutions }
import uk.gov.hmrc.gform.gform.Substituter
import SmartStringIfBranchSubstituter._
import SmartStringIfReplaceSubstituter._
import scala.jdk.CollectionConverters._
import java.text.MessageFormat

case class SmartString(localised: LocalisedString, interpolations: List[Expr]) {
  def replace(toReplace: String, replaceWith: String): SmartString = {
    val substitutions = SmartStringIfReplaceSubstitutions(toReplace, replaceWith)
    val substituter = implicitly[Substituter[SmartStringIfReplaceSubstitutions, Expr]]
    val updatedInterpolations = interpolations.map { case expr =>
      substituter.substitute(substitutions, expr)
    }
    copy(localised = localised.replace(toReplace, replaceWith), interpolations = updatedInterpolations)
  }

  def rawValue(implicit l: LangADT): String = localised.value(l)

  def isEmpty(implicit l: LangADT): Boolean = rawValue.isEmpty

  def expand(index: Int, baseIds: List[FormComponentId]) = ExpandUtils.expandSmartString(this, index, baseIds)

  def expandDataRetrieve(index: Int) = ExpandUtils.expandDataRetrieve(this, index)

  def valueWithoutInterpolations(implicit l: LangADT): String = {
    import scala.jdk.CollectionConverters._
    new MessageFormat(rawValue(l)).format(interpolations.map(_ => "").asJava.toArray)
  }

  def transform(fEn: String => String, fCy: String => String): SmartString =
    copy(localised = localised.transform(fEn, fCy))

  /*
   * This method replaces SmartStringIf expressions within interpolations with expressions from the Concat in the "true" branch.
   * It utilizes "Constant" expressions to reconstruct the raw value. Additionally, the method renumbers placeholders as necessary
   * to preserve the correct sequence.
   */
  def resolveSmartStringIf(resolve: BooleanExpr => Boolean): SmartString = {
    val substitutions = SmartStringIfBranchSubstitutions(resolve)
    val substituter = implicitly[Substituter[SmartStringIfBranchSubstitutions, Expr]]
    val (indexedStringReplacements, newInterpolations) =
      interpolations
        .map(substituter.substitute(substitutions, _))
        .zipWithIndex
        .flatMap {
          case (Concat(exprs), index) => exprs.map(e => (index, e))
          case (expr, index)          => List((index, expr))
        }
        .foldLeft((List.empty[(Int, String)], List.empty[Expr])) {
          case ((indexedReplacements, exprs), (index, Constant(value))) =>
            ((index, value) :: indexedReplacements, exprs)
          case ((indexedReplacements, exprs), (index, expr)) =>
            ((index -> s"{${exprs.length}}") :: indexedReplacements, expr :: exprs)
        }

    val intermediateInterpolations = indexedStringReplacements.reverse
      .groupBy(_._1)
      .toList
      .sortBy(_._1)
      .map(_._2.map(_._2).mkString)

    val newLocalised = localised.m.view.mapValues(
      new MessageFormat(_)
        .format(intermediateInterpolations.asJava.toArray)
    )
    SmartString(LocalisedString(newLocalised.toMap), newInterpolations.reverse)
  }
}

object SmartString {
  val empty: SmartString = SmartString(LocalisedString.empty, Nil)

  val blank: SmartString = SmartString(LocalisedString(Map(LangADT.En -> "", LangADT.Cy -> "")), Nil)

  implicit val format: Format[SmartString] = Json.format[SmartString]
}
