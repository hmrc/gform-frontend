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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.Eq

import java.time.LocalDate
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveId }

sealed trait Expr extends Product with Serializable {

  def booleanExprs(): List[BooleanExpr] = collect {
    case Add(field1: Expr, field2: Expr)          => field1.booleanExprs() ++ field2.booleanExprs()
    case Multiply(field1: Expr, field2: Expr)     => field1.booleanExprs() ++ field2.booleanExprs()
    case Subtraction(field1: Expr, field2: Expr)  => field1.booleanExprs() ++ field2.booleanExprs()
    case Divide(field1: Expr, field2: Expr)       => field1.booleanExprs() ++ field2.booleanExprs()
    case HideZeroDecimals(field1: Expr)           => field1.booleanExprs()
    case IfElse(cond, field1: Expr, field2: Expr) => cond :: field1.booleanExprs() ++ field2.booleanExprs()
    case Else(field1: Expr, field2: Expr)         => field1.booleanExprs() ++ field2.booleanExprs()
    case Sum(field1: Expr)                        => field1.booleanExprs()
    case DateCtx(dateExpr)                        => dateExpr.booleanExprs()
    case DateFunction(dateFunc)                   => dateFunc.dateExpr.booleanExprs()
    case Period(dateCtx1, dateCtx2, _)            => dateCtx1.booleanExprs() ::: dateCtx2.booleanExprs()
    case Between(dateCtx1, dateCtx2, _)           => dateCtx1.booleanExprs() ::: dateCtx2.booleanExprs()
    case Typed(expr, _)                           => expr.booleanExprs()
    case IndexOfDataRetrieveCtx(_, expr)          => expr.booleanExprs()
    case StringOps(expr, _)                       => expr.booleanExprs()
    case Concat(exprs)                            => exprs.flatMap(_.booleanExprs())
    case LookupOps(expr, _)                       => expr.booleanExprs()
  }.flatten

  def prettyPrint: String = ExprPrettyPrint.prettyPrintExpr(this)

  def staticType(): Expr = this match {
    case Add(field1: Expr, field2: Expr)         => field1.staticType()
    case Multiply(field1: Expr, field2: Expr)    => field1.staticType()
    case Subtraction(field1: Expr, field2: Expr) => field1.staticType()
    case Divide(field1: Expr, field2: Expr)      => field1.staticType()
    case HideZeroDecimals(field1: Expr)          => field1.staticType()
    case IfElse(_, field1: Expr, field2: Expr)   => field1.staticType()
    case Else(field1: Expr, field2: Expr)        => field1.staticType()
    case _                                       => this
  }

  def collect[T](pf: PartialFunction[Expr, T]): List[T] = this match {
    case expr if pf.isDefinedAt(expr)            => List(pf(expr))
    case Add(field1: Expr, field2: Expr)         => field1.collect(pf) ++ field2.collect(pf)
    case Multiply(field1: Expr, field2: Expr)    => field1.collect(pf) ++ field2.collect(pf)
    case Subtraction(field1: Expr, field2: Expr) => field1.collect(pf) ++ field2.collect(pf)
    case Divide(field1: Expr, field2: Expr)      => field1.collect(pf) ++ field2.collect(pf)
    case HideZeroDecimals(field1: Expr)          => field1.collect(pf)
    case Sum(field1: Expr)                       => field1.collect(pf)
    case IfElse(cond, field1, field2) =>
      cond.allExpressions.flatMap(_.collect(pf)) ++ field1.collect(pf) ++ field2.collect(pf)
    case Else(field1: Expr, field2: Expr)    => field1.collect(pf) ++ field2.collect(pf)
    case Period(dateCtx1, dateCtx2, _)       => dateCtx1.collect(pf) ::: dateCtx2.collect(pf)
    case Between(dateCtx1, dateCtx2, _)      => dateCtx1.collect(pf) ::: dateCtx2.collect(pf)
    case Typed(expr, _)                      => expr.collect(pf)
    case IndexOfDataRetrieveCtx(_, expr)     => expr.collect(pf)
    case StringOps(expr, _)                  => expr.collect(pf)
    case Concat(exprs)                       => exprs.flatMap(_.collect(pf))
    case LookupOps(expr, _)                  => expr.collect(pf)
    case DateCtx(value: DateExpr)            => value.collect(pf)
    case DateFunction(value: DateProjection) => value.dateExpr.collect(pf)
    case _                                   => Nil
  }

  def allAuthCtx(): List[AuthCtx] = collect { case e: AuthCtx => e }.collect { case e: AuthCtx => e }
  def allAddressLens(): List[AddressLens] = collect { case e: AddressLens => e }.collect { case e: AddressLens => e }
  def allTaskStatuses(): List[TaskStatus] = collect { case e: TaskStatus => e }.collect { case e: TaskStatus => e }

  def allFormComponentIds(): List[FormComponentId] = collect {
    case FormCtx(formComponentId)                        => formComponentId
    case Count(formComponentId)                          => formComponentId
    case Index(formComponentId)                          => formComponentId
    case AddressLens(formComponentId, _)                 => formComponentId
    case LookupColumn(formComponentId, _)                => formComponentId
    case CsvCountryCountCheck(formComponentId, _, _)     => formComponentId
    case Size(formComponentId, _)                        => formComponentId
    case IndexOf(formComponentId, _)                     => formComponentId
    case IndexOfInChoice(_, formComponentId)             => formComponentId
    case NumberedList(formComponentId)                   => formComponentId
    case BulletedList(formComponentId)                   => formComponentId
    case NumberedListChoicesSelected(formComponentId, _) => formComponentId
    case BulletedListChoicesSelected(formComponentId, _) => formComponentId
    case ChoicesRevealedField(formComponentId)           => formComponentId
    case ChoicesSelected(formComponentId)                => formComponentId
    case ChoicesAvailable(formComponentId, _)            => formComponentId
    case CountSelectedChoices(formComponentId)           => formComponentId
    case ChoicesCount(formComponentId)                   => formComponentId
    case DisplayAsEntered(formComponentId)               => formComponentId
  }

  def allFormCtxIds(): List[FormComponentId] = collect { case FormCtx(formComponentId) =>
    formComponentId
  }
}

final case class Add(field1: Expr, field2: Expr) extends Expr
final case class Multiply(field1: Expr, field2: Expr) extends Expr
final case class Subtraction(field1: Expr, field2: Expr) extends Expr
final case class Divide(field1: Expr, field2: Expr) extends Expr
final case class HideZeroDecimals(expr: Expr) extends Expr
final case class IfElse(cond: BooleanExpr, field1: Expr, field2: Expr) extends Expr
final case class Else(field1: Expr, field2: Expr) extends Expr
final case class FormCtx(formComponentId: FormComponentId) extends Expr
final case class Sum(field1: Expr) extends Expr
final case class Count(formComponentId: FormComponentId) extends Expr
final case class Index(formComponentId: FormComponentId) extends Expr
final case class ParamCtx(queryParam: QueryParam) extends Expr
final case class AuthCtx(value: AuthInfo) extends Expr
final case class UserCtx(value: UserField) extends Expr
final case class Constant(value: String) extends Expr
final case class LinkCtx(link: InternalLink) extends Expr
final case object Value extends Expr
final case class FormTemplateCtx(value: FormTemplateProp) extends Expr
final case class DateCtx(value: DateExpr) extends Expr
final case class DateFunction(value: DateProjection) extends Expr
final case class AddressLens(formComponentId: FormComponentId, detail: AddressDetail) extends Expr
final case class PeriodValue(value: String) extends Expr
final case class Period(dateCtx1: Expr, dateCtx2: Expr, func: PeriodFn) extends Expr
final case class Between(dateCtx1: Expr, dateCtx2: Expr, measurementType: MeasurementType) extends Expr
final case object LangCtx extends Expr
final case class DataRetrieveCtx(id: DataRetrieveId, attribute: DataRetrieve.Attribute) extends Expr
final case class DataRetrieveCount(id: DataRetrieveId) extends Expr
final case class LookupColumn(formComponentId: FormComponentId, column: String) extends Expr
final case class LookupOps(expr: Expr, lookupFnc: LookupFnc) extends Expr
final case class CsvCountryCountCheck(formComponentId: FormComponentId, column: String, value: String) extends Expr
final case class Size(formComponentId: FormComponentId, index: SizeRefType) extends Expr
final case class Typed(expr: Expr, tpe: ExplicitExprType) extends Expr
final case class IndexOf(formComponentId: FormComponentId, index: Int) extends Expr
final case class IndexOfInChoice(value: OptionDataValue, formComponentId: FormComponentId) extends Expr
final case class IndexOfDataRetrieveCtx(ctx: DataRetrieveCtx, expr: Expr) extends Expr
final case class NumberedList(formComponentId: FormComponentId) extends Expr
final case class BulletedList(formComponentId: FormComponentId) extends Expr
final case class NumberedListChoicesSelected(formComponentId: FormComponentId, insideAtl: Option[Boolean]) extends Expr
final case class BulletedListChoicesSelected(formComponentId: FormComponentId, insideAtl: Option[Boolean]) extends Expr
final case class StringOps(field1: Expr, stringFnc: StringFnc) extends Expr
final case class Concat(exprs: List[Expr]) extends Expr
final case object CountryOfItmpAddress extends Expr
final case class ChoicesRevealedField(formComponentId: FormComponentId) extends Expr
final case class ChoicesSelected(formComponentId: FormComponentId) extends Expr
final case class ChoicesAvailable(formComponentId: FormComponentId, insideAtl: Option[Boolean]) extends Expr
final case class CountSelectedChoices(formComponentId: FormComponentId) extends Expr
final case class ChoicesCount(formComponentId: FormComponentId) extends Expr
final case class TaskStatus(taskId: TaskId) extends Expr
final case class DisplayAsEntered(formComponentId: FormComponentId) extends Expr

sealed trait DateProjection extends Product with Serializable {
  def dateExpr: DateExpr

  def toValue(localDate: LocalDate): Int =
    this match {
      case DateProjection.Day(_)   => localDate.getDayOfMonth()
      case DateProjection.Month(_) => localDate.getMonthValue()
      case DateProjection.Year(_)  => localDate.getYear()
      case DateProjection.TaxYear(_) =>
        localDate.getMonthValue match {
          case m if m == 4 => if (localDate.getDayOfMonth < 6) localDate.getYear else localDate.getYear + 1
          case m if m < 4  => localDate.getYear
          case m if m > 4  => localDate.getYear + 1
        }
    }
}

object DateProjection {

  case class Day(dateExpr: DateExpr) extends DateProjection
  case class Month(dateExpr: DateExpr) extends DateProjection
  case class Year(dateExpr: DateExpr) extends DateProjection
  case class TaxYear(dateExpr: DateExpr) extends DateProjection

  implicit val format: OFormat[DateProjection] = derived.oformat()
}

sealed trait SizeRefType extends Product with Serializable
object SizeRefType {
  case class IndexBased(index: Int) extends SizeRefType
  case class ValueBased(value: String) extends SizeRefType

  implicit val format: OFormat[SizeRefType] = derived.oformat()
}

sealed trait ExplicitExprType extends Product with Serializable
object ExplicitExprType {
  case object Text extends ExplicitExprType
  case class Sterling(
    roundingMode: RoundingMode
  ) extends ExplicitExprType
  case class WholeSterling(
    roundingMode: RoundingMode
  ) extends ExplicitExprType
  case class Number(
    fractionalDigits: Int,
    roundingMode: RoundingMode
  ) extends ExplicitExprType

  implicit val format: OFormat[ExplicitExprType] = derived.oformat()
}

sealed trait PeriodFn
object PeriodFn {
  case object Identity extends PeriodFn
  case object Sum extends PeriodFn
  case object TotalMonths extends PeriodFn
  case object Years extends PeriodFn
  case object Months extends PeriodFn
  case object Days extends PeriodFn
  implicit val format: OFormat[PeriodFn] = derived.oformat()
}

sealed trait MeasurementType
object MeasurementType {
  case object Weeks extends MeasurementType
  case object Days extends MeasurementType
  implicit val format: OFormat[MeasurementType] = derived.oformat()
}

sealed trait StringFnc {
  def eval(s: String): String = this match {
    case StringFnc.Capitalize     => s.capitalize
    case StringFnc.CapitalizeAll  => s.split(' ').map(_.capitalize).mkString(" ")
    case StringFnc.LowerCase      => s.toLowerCase
    case StringFnc.UpperCase      => s.toUpperCase
    case StringFnc.RemoveSpaces   => s.replaceAll(" ", "")
    case StringFnc.LowerCaseFirst => s.headOption.map(c => s"${c.toLower}${s.tail}").getOrElse("")
    case StringFnc.SubString(beginIndex, endIndex) =>
      s.substring(Math.min(beginIndex, s.length), Math.min(endIndex, s.length))
  }
}

object StringFnc {
  case object Capitalize extends StringFnc
  case object CapitalizeAll extends StringFnc
  case object UpperCase extends StringFnc
  case object LowerCase extends StringFnc
  case object RemoveSpaces extends StringFnc
  case object LowerCaseFirst extends StringFnc
  case class SubString(beginIndex: Int, endIndex: Int) extends StringFnc
  implicit val format: OFormat[StringFnc] = derived.oformat()
}

sealed trait UserFieldFunc
object UserFieldFunc {
  case object Count extends UserFieldFunc
  case class Index(i: Int) extends UserFieldFunc

  implicit val format: OFormat[UserFieldFunc] = derived.oformat()
}

sealed trait AddressDetail {
  def toAddressAtom: Atom = this match {
    case AddressDetail.Line1    => Address.street1
    case AddressDetail.Line2    => Address.street2
    case AddressDetail.Line3    => Address.street3
    case AddressDetail.Line4    => Address.street4
    case AddressDetail.Postcode => Address.postcode
    case AddressDetail.Country  => Address.country
  }

  def toOverseasAddressAtom: Atom = this match {
    case AddressDetail.Line1    => OverseasAddress.line1
    case AddressDetail.Line2    => OverseasAddress.line2
    case AddressDetail.Line3    => OverseasAddress.line3
    case AddressDetail.Line4    => OverseasAddress.city
    case AddressDetail.Postcode => OverseasAddress.postcode
    case AddressDetail.Country  => OverseasAddress.country
  }
}

object AddressDetail {
  case object Line1 extends AddressDetail
  case object Line2 extends AddressDetail
  case object Line3 extends AddressDetail
  case object Line4 extends AddressDetail
  case object Postcode extends AddressDetail
  case object Country extends AddressDetail

  implicit val format: OFormat[AddressDetail] = derived.oformat()
}

object FormCtx {
  implicit val format: OFormat[FormCtx] = derived.oformat()
}

object Expr {
  val additionIdentity: Expr = Constant("0")
  implicit val dataRetrieveCtxFormat: OFormat[DataRetrieveCtx] = derived.oformat()
  implicit val format: OFormat[Expr] = derived.oformat()
  implicit val equal: Eq[Expr] = Eq.fromUniversalEquals
}

sealed trait UserField {
  def fold[B](
    f: UserField.AffinityGroup.type => B
  )(g: UserField.Enrolment => B)(h: UserField.EnrolledIdentifier.type => B)(i: UserField.CredentialRole.type => B): B =
    this match {
      case UserField.AffinityGroup      => f(UserField.AffinityGroup)
      case e: UserField.Enrolment       => g(e)
      case UserField.EnrolledIdentifier => h(UserField.EnrolledIdentifier)
      case UserField.CredentialRole     => i(UserField.CredentialRole)
    }
}

object UserField {
  final case object AffinityGroup extends UserField
  final case class Enrolment(serviceName: ServiceName, identifierName: IdentifierName, func: Option[UserFieldFunc])
      extends UserField
  final case object EnrolledIdentifier extends UserField
  final case object CredentialRole extends UserField

  implicit val format: OFormat[UserField] = derived.oformat()
}

final case class ServiceName(value: String) extends AnyVal
object ServiceName {
  implicit val format: OFormat[ServiceName] = derived.oformat()
}

final case class IdentifierName(value: String) extends AnyVal
object IdentifierName {
  implicit val format: OFormat[IdentifierName] = derived.oformat()
}

sealed trait ItmpNameFocus

object ItmpNameFocus {
  case object GivenName extends ItmpNameFocus
  case object MiddleName extends ItmpNameFocus
  case object FamilyName extends ItmpNameFocus

  implicit val format: OFormat[ItmpNameFocus] = derived.oformat()
}

sealed trait AuthInfo extends Product with Serializable

object AuthInfo {

  final case object GG extends AuthInfo
  final case object PayeNino extends AuthInfo
  final case object EmailId extends AuthInfo
  final case object SaUtr extends AuthInfo
  final case object CtUtr extends AuthInfo
  final case object EtmpRegistrationNumber extends AuthInfo
  final case object ItmpName extends AuthInfo
  final case class ItmpNameLens(focus: ItmpNameFocus) extends AuthInfo
  final case object ItmpDateOfBirth extends AuthInfo
  final case object ItmpAddress extends AuthInfo
  final case object PayeRef extends AuthInfo
  final case object Vrn extends AuthInfo

  implicit val format: OFormat[AuthInfo] = derived.oformat()

}

sealed trait FormTemplateProp extends Product with Serializable
object FormTemplateProp {
  case object Id extends FormTemplateProp
  case object SubmissionReference extends FormTemplateProp
  case object FileSizeLimit extends FormTemplateProp

  implicit val format: OFormat[FormTemplateProp] = derived.oformat()
}

sealed trait HmrcTaxPeriodInfo
object HmrcTaxPeriodInfo {
  case object PeriodTo extends HmrcTaxPeriodInfo
  case object PeriodFrom extends HmrcTaxPeriodInfo
  case object PeriodDue extends HmrcTaxPeriodInfo

  implicit val format: OFormat[HmrcTaxPeriodInfo] = derived.oformat()
}

sealed trait LoginInfo

object LoginInfo {
  final case object EmailLogin extends LoginInfo
  final case object GGLogin extends LoginInfo

  implicit val format: OFormat[LoginInfo] = derived.oformat()
}

sealed trait LookupFnc
object LookupFnc {
  case object CountryName extends LookupFnc
  case object SicDescription extends LookupFnc
  implicit val format: OFormat[LookupFnc] = derived.oformat()
}
