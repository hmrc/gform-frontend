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

import play.api.libs.json.{ Format, Json, OFormat }
import uk.gov.hmrc.gform.models.ExpandUtils
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BooleanExpr, Expr, FormComponentId }
import julienrf.json.derived

import java.text.MessageFormat

case class SmartStringInternal(localised: LocalisedString, interpolations: List[Expr]) {
  def replace(toReplace: String, replaceWith: String): SmartStringInternal =
    copy(localised = localised.replace(toReplace, replaceWith))

  def rawValue(implicit l: LangADT): String = localised.value(l)

  def isEmpty(implicit l: LangADT): Boolean = rawValue.isEmpty

  def transform(fEn: String => String, fCy: String => String): SmartStringInternal =
    copy(localised = localised.transform(fEn, fCy))
}

object SmartStringInternal {
  val empty: SmartStringInternal = SmartStringInternal(LocalisedString.empty, Nil)

  val blank: SmartStringInternal = SmartStringInternal(LocalisedString(Map(LangADT.En -> "", LangADT.Cy -> "")), Nil)

  implicit val format: Format[SmartStringInternal] = Json.format[SmartStringInternal]
}

sealed trait SmartString {
  private def fold[B](f: SmartString.SmartStringBase => B)(g: SmartString.SmartStringCond => B): B =
    this match {
      case n: SmartString.SmartStringBase => f(n)
      case r: SmartString.SmartStringCond => g(r)
    }

  private def resolvedInternal(resolver: BooleanExpr => Boolean) = fold[SmartStringInternal](ssb => ssb.internal)(ssc =>
    ssc.ifConditions.find { case (b, _) => resolver(b) }.map(_._2).getOrElse(ssc.elseCondition)
  )

  def expand(index: Int, baseIds: List[FormComponentId]) = ExpandUtils.expandSmartString(this, index, baseIds)

  def expandDataRetrieve(index: Int) = ExpandUtils.expandDataRetrieve(this, index)

  def updateInterpolations(f: Expr => Expr, g: BooleanExpr => BooleanExpr): SmartString =
    fold[SmartString](ssb =>
      SmartString.SmartStringBase(ssb.internal.copy(interpolations = ssb.internal.interpolations.map(f)))
    )(ssc =>
      SmartString.SmartStringCond(
        ssc.ifConditions.map { case (b, ssi) => (g(b), ssi.copy(interpolations = ssi.interpolations.map(f))) },
        ssc.elseCondition.copy(interpolations = ssc.elseCondition.interpolations.map(f))
      )
    )

  def replace(toReplace: String, replaceWith: String): SmartString =
    fold[SmartString](ssb => SmartString.SmartStringBase(ssb.internal.replace(toReplace, replaceWith)))(ssc =>
      SmartString.SmartStringCond(
        ssc.ifConditions.map { case (b, ssi) => (b, ssi.replace(toReplace, replaceWith)) },
        ssc.elseCondition.replace(toReplace, replaceWith)
      )
    )

  def valueWithoutInterpolations(resolver: BooleanExpr => Boolean)(implicit l: LangADT): String = {
    import scala.jdk.CollectionConverters._
    val internal = resolvedInternal(resolver)
    new MessageFormat(internal.rawValue(l)).format(internal.interpolations.map(_ => "").asJava.toArray)
  }

  def isEmpty(resolver: BooleanExpr => Boolean)(implicit l: LangADT): Boolean = resolvedInternal(resolver).isEmpty

  def interpolations(resolver: BooleanExpr => Boolean): List[Expr] = resolvedInternal(resolver).interpolations
  def localised(resolver: BooleanExpr => Boolean): LocalisedString = resolvedInternal(resolver).localised

  def allInterpolations: List[Expr] = fold[List[Expr]](ssb => ssb.internal.interpolations)(ssc =>
    ssc.ifConditions.flatMap { case (_, ssi) => ssi.interpolations } ++ ssc.elseCondition.interpolations
  )

  def rawValue(resolver: BooleanExpr => Boolean)(implicit l: LangADT): String =
    resolvedInternal(resolver).rawValue(l)
  def rawDefaultValue(implicit l: LangADT): String = resolvedInternal(_ => false).rawValue(l)

  def transform(fEn: String => String, fCy: String => String): SmartString =
    fold[SmartString](ssb => SmartString.SmartStringBase(ssb.internal.transform(fEn, fCy)))(ssc =>
      SmartString.SmartStringCond(
        ssc.ifConditions.map { case (b, ssi) => (b, ssi.transform(fEn, fCy)) },
        ssc.elseCondition.transform(fEn, fCy)
      )
    )

}

object SmartString {

  case class SmartStringBase(internal: SmartStringInternal) extends SmartString
  case class SmartStringCond(ifConditions: List[(BooleanExpr, SmartStringInternal)], elseCondition: SmartStringInternal)
      extends SmartString

  val empty: SmartString = SmartStringBase(SmartStringInternal(LocalisedString.empty, Nil))

  val blank: SmartString = SmartStringBase(
    SmartStringInternal(LocalisedString(Map(LangADT.En -> "", LangADT.Cy -> "")), Nil)
  )

  def apply(localised: LocalisedString, interpolations: List[Expr]): SmartString =
    SmartStringBase(SmartStringInternal(localised, interpolations))

  implicit val smartStringFormat: OFormat[SmartString] = derived.oformat()
}
