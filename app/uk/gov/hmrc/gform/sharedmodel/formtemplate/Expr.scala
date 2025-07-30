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
import uk.gov.hmrc.gform.models.{ FormModel, PageMode }
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveId }

sealed trait Expr extends Product with Serializable {

  def firstExprForTypeResolution[T <: PageMode](formModel: FormModel[T]): Option[Expr] = {
    def loop(expr: Expr): List[Expr] = expr match {
      case Add(field1: Expr, field2: Expr)         => loop(field1) ++ loop(field2)
      case Multiply(field1: Expr, field2: Expr)    => loop(field1) ++ loop(field2)
      case Subtraction(field1: Expr, field2: Expr) => loop(field1) ++ loop(field2)
      case Divide(field1: Expr, field2: Expr)      => loop(field1) ++ loop(field2)
      case HideZeroDecimals(field1: Expr)          => loop(field1)
      case IfElse(_, field1: Expr, field2: Expr)   => loop(field1) ++ loop(field2)
      case Else(field1: Expr, field2: Expr)        => loop(field1) ++ loop(field2)
      case FormCtx(_)                              => expr :: Nil
      case Sum(field1: Expr) =>
        field1 match {
          case FormCtx(formComponentId) =>
            val exprs = formModel.allFormComponents.collect {
              case fc if fc.baseComponentId == formComponentId.baseComponentId => FormCtx(fc.id)
            }
            if (exprs.isEmpty) {
              // Indicates that the specified formComponentId does not exist within the form model, implying no instances across iterations.
              // Therefore, the original formComponentId is utilized for type resolution.
              FormCtx(formComponentId) :: Nil
            } else {
              exprs
            }
          case _ => loop(field1)
        }
      case Count(formComponentId: FormComponentId) => expr :: Nil
      case Index(formComponentId: FormComponentId) => FormCtx(formComponentId.withFirstIndex) :: Nil
      case AuthCtx(_)                              => expr :: Nil
      case UserCtx(_)                              => expr :: Nil
      case Constant(_)                             => expr :: Nil
      case PeriodValue(_)                          => expr :: Nil
      case Value                                   => expr :: Nil
      case FormTemplateCtx(_)                      => expr :: Nil
      case ParamCtx(_)                             => expr :: Nil
      case LinkCtx(_)                              => expr :: Nil
      case LangCtx                                 => expr :: Nil
      case DateCtx(dateExpr)                       => dateExpr.leafExprs
      case DateFunction(_)                         => expr :: Nil
      case Period(_, _)                            => expr :: Nil
      case Between(_, _, _)                        => expr :: Nil
      case PeriodExt(_, _)                         => expr :: Nil
      case AddressLens(_, _)                       => expr :: Nil
      case DataRetrieveCtx(_, _)                   => expr :: Nil
      case DataRetrieveCount(_)                    => expr :: Nil
      case LookupColumn(_, _)                      => expr :: Nil
      case CsvCountryCountCheck(_, _, _)           => expr :: Nil
      case Size(_, _)                              => expr :: Nil
      case Typed(e, _)                             => expr :: Nil
      case IndexOf(_, _)                           => expr :: Nil
      case IndexOfDataRetrieveCtx(_, _)            => expr :: Nil
      case NumberedList(_)                         => expr :: Nil
      case BulletedList(_)                         => expr :: Nil
      case StringOps(_, _)                         => expr :: Nil
      case Concat(_)                               => expr :: Nil
      case CountryOfItmpAddress                    => expr :: Nil
      case ChoicesRevealedField(_)                 => expr :: Nil
      case ChoicesSelected(_)                      => expr :: Nil
      case ChoicesAvailable(_)                     => expr :: Nil
      case CountSelectedChoices(_)                 => expr :: Nil
      case TaskStatus(_)                           => expr :: Nil
      case LookupOps(_, _)                         => expr :: Nil

    }
    loop(this).headOption
  }

  def prettyPrint: String = ExprPrettyPrint.prettyPrintExpr(this)

  def leafs[T <: PageMode](formModel: FormModel[T]): List[Expr] = this match {
    case Add(field1: Expr, field2: Expr)         => field1.leafs(formModel) ++ field2.leafs(formModel)
    case Multiply(field1: Expr, field2: Expr)    => field1.leafs(formModel) ++ field2.leafs(formModel)
    case Subtraction(field1: Expr, field2: Expr) => field1.leafs(formModel) ++ field2.leafs(formModel)
    case Divide(field1: Expr, field2: Expr)      => field1.leafs(formModel) ++ field2.leafs(formModel)
    case HideZeroDecimals(field1: Expr)          => field1.leafs(formModel)
    case IfElse(cond, field1: Expr, field2: Expr) =>
      cond.allExpressions.flatMap(_.leafs(formModel)) ++
        field1.leafs(formModel) ++ field2.leafs(formModel)
    case Else(field1: Expr, field2: Expr)          => field1.leafs(formModel) ++ field2.leafs(formModel)
    case FormCtx(formComponentId: FormComponentId) => this :: Nil
    case Sum(field1: Expr) =>
      field1 match {
        case FormCtx(formComponentId) =>
          formModel.allFormComponents.collect {
            case fc if fc.baseComponentId == formComponentId.baseComponentId => FormCtx(fc.id)
          }
        case _ => field1.leafs(formModel)
      }
    case Count(formComponentId: FormComponentId) => FormCtx(formComponentId.withFirstIndex) :: Nil
    case Index(formComponentId: FormComponentId) => FormCtx(formComponentId.withFirstIndex) :: Nil
    case AuthCtx(value: AuthInfo)                => this :: Nil
    case UserCtx(value: UserField)               => this :: Nil
    case Constant(value: String)                 => this :: Nil
    case PeriodValue(value: String)              => this :: Nil
    // case HmrcRosmRegistrationCheck(value: RosmProp) => this :: Nil
    case Value                                    => this :: Nil
    case LangCtx                                  => this :: Nil
    case FormTemplateCtx(value: FormTemplateProp) => this :: Nil
    case ParamCtx(_)                              => this :: Nil
    case LinkCtx(_)                               => this :: Nil
    case DateCtx(dateExpr)                        => dateExpr.leafExprs
    case DateFunction(dateFunc)                   => dateFunc.dateExpr.leafExprs
    case Period(dateCtx1, dateCtx2)               => dateCtx1.leafs(formModel) ::: dateCtx2.leafs(formModel)
    case PeriodExt(periodFun, _)                  => periodFun.leafs(formModel)
    case Between(dateCtx1, dateCtx2, _)           => dateCtx1.leafs(formModel) ::: dateCtx2.leafs(formModel)
    case AddressLens(formComponentId, _)          => this :: Nil
    case DataRetrieveCtx(_, _)                    => this :: Nil
    case DataRetrieveCount(_)                     => this :: Nil
    case LookupColumn(_, _)                       => this :: Nil
    case CsvCountryCountCheck(_, _, _)            => this :: Nil
    case Size(_, _)                               => this :: Nil
    case Typed(expr, _)                           => expr.leafs(formModel)
    case IndexOf(formComponentId, _)              => FormCtx(formComponentId) :: Nil
    case IndexOfDataRetrieveCtx(_, _)             => this :: Nil
    case NumberedList(formComponentId)            => FormCtx(formComponentId) :: Nil
    case BulletedList(formComponentId)            => FormCtx(formComponentId) :: Nil
    case StringOps(expr, _)                       => expr.leafs(formModel)
    case Concat(exprs)                            => exprs.flatMap(_.leafs(formModel))
    case CountryOfItmpAddress                     => this :: Nil
    case ChoicesRevealedField(formComponentId)    => FormCtx(formComponentId) :: Nil
    case ChoicesSelected(formComponentId)         => FormCtx(formComponentId) :: Nil
    case ChoicesAvailable(formComponentId)        => FormCtx(formComponentId) :: Nil
    case CountSelectedChoices(formComponentId)    => FormCtx(formComponentId) :: Nil
    case TaskStatus(_)                            => this :: Nil
    case LookupOps(expr, _)                       => expr.leafs(formModel)
  }

  def sums: List[Sum] = this match {
    case Add(field1: Expr, field2: Expr)           => field1.sums ++ field2.sums
    case Multiply(field1: Expr, field2: Expr)      => field1.sums ++ field2.sums
    case Subtraction(field1: Expr, field2: Expr)   => field1.sums ++ field2.sums
    case Divide(field1: Expr, field2: Expr)        => field1.sums ++ field2.sums
    case HideZeroDecimals(field1: Expr)            => field1.sums
    case IfElse(cond, field1: Expr, field2: Expr)  => cond.allExpressions.flatMap(_.sums) ++ field1.sums ++ field2.sums
    case Else(field1: Expr, field2: Expr)          => field1.sums ++ field2.sums
    case FormCtx(formComponentId: FormComponentId) => Nil
    case sum @ Sum(field1: Expr)                   => sum :: Nil
    case Count(field1: FormComponentId)            => Nil
    case Index(field1: FormComponentId)            => Nil
    case AuthCtx(value: AuthInfo)                  => Nil
    case UserCtx(value: UserField)                 => Nil
    case Constant(value: String)                   => Nil
    case PeriodValue(value: String)                => Nil
    // case HmrcRosmRegistrationCheck(value: RosmProp) => Nil
    case Value                                    => Nil
    case FormTemplateCtx(value: FormTemplateProp) => Nil
    case ParamCtx(_)                              => Nil
    case LinkCtx(_)                               => Nil
    case LangCtx                                  => Nil
    case DateCtx(_)                               => Nil
    case DateFunction(_)                          => Nil
    case Period(_, _)                             => Nil
    case PeriodExt(_, _)                          => Nil
    case Between(_, _, _)                         => Nil
    case AddressLens(_, _)                        => Nil
    case DataRetrieveCtx(_, _)                    => Nil
    case DataRetrieveCount(_)                     => Nil
    case LookupColumn(_, _)                       => Nil
    case CsvCountryCountCheck(_, _, _)            => Nil
    case Size(_, _)                               => Nil
    case Typed(expr, _)                           => expr.sums
    case IndexOf(_, _)                            => Nil
    case IndexOfDataRetrieveCtx(_, _)             => Nil
    case NumberedList(_)                          => Nil
    case BulletedList(_)                          => Nil
    case StringOps(_, _)                          => Nil
    case Concat(_)                                => Nil
    case CountryOfItmpAddress                     => Nil
    case ChoicesRevealedField(_)                  => Nil
    case ChoicesSelected(_)                       => Nil
    case ChoicesAvailable(_)                      => Nil
    case CountSelectedChoices(_)                  => Nil
    case TaskStatus(_)                            => Nil
    case LookupOps(_, _)                          => Nil
  }

  def leafs(): List[Expr] = this match {
    case Add(field1: Expr, field2: Expr)         => field1.leafs() ++ field2.leafs()
    case Multiply(field1: Expr, field2: Expr)    => field1.leafs() ++ field2.leafs()
    case Subtraction(field1: Expr, field2: Expr) => field1.leafs() ++ field2.leafs()
    case Divide(field1: Expr, field2: Expr)      => field1.leafs() ++ field2.leafs()
    case HideZeroDecimals(field1: Expr)          => field1.leafs()
    case IfElse(cond, field1: Expr, field2: Expr) =>
      cond.allExpressions.flatMap(_.leafs()) ++
        field1.leafs() ++ field2.leafs()
    case Else(field1: Expr, field2: Expr)          => field1.leafs() ++ field2.leafs()
    case FormCtx(formComponentId: FormComponentId) => this :: Nil
    case Sum(field1: Expr)                         => field1.leafs()
    case Count(formComponentId: FormComponentId)   => FormCtx(formComponentId.withFirstIndex) :: Nil
    case Index(formComponentId: FormComponentId)   => FormCtx(formComponentId.withFirstIndex) :: Nil
    case AuthCtx(value: AuthInfo)                  => this :: Nil
    case UserCtx(value: UserField)                 => this :: Nil
    case Constant(value: String)                   => this :: Nil
    case PeriodValue(value: String)                => this :: Nil
    case Value                                     => this :: Nil
    case LangCtx                                   => this :: Nil
    case FormTemplateCtx(value: FormTemplateProp)  => this :: Nil
    case ParamCtx(_)                               => this :: Nil
    case LinkCtx(_)                                => this :: Nil
    case DateCtx(dateExpr)                         => dateExpr.leafExprs
    case DateFunction(dateFunc)                    => dateFunc.dateExpr.leafExprs
    case Period(dateCtx1, dateCtx2)                => dateCtx1.leafs() ::: dateCtx2.leafs()
    case PeriodExt(periodFun, _)                   => periodFun.leafs()
    case Between(dateCtx1, dateCtx2, _)            => dateCtx1.leafs() ::: dateCtx2.leafs()
    case AddressLens(formComponentId, _)           => this :: Nil
    case DataRetrieveCtx(_, _)                     => this :: Nil
    case DataRetrieveCount(_)                      => this :: Nil
    case LookupColumn(_, _)                        => this :: Nil
    case CsvCountryCountCheck(_, _, _)             => this :: Nil
    case Size(_, _)                                => this :: Nil
    case Typed(expr, _)                            => expr.leafs()
    case IndexOf(formComponentId, _)               => FormCtx(formComponentId) :: Nil
    case IndexOfDataRetrieveCtx(_, _)              => this :: Nil
    case NumberedList(formComponentId)             => FormCtx(formComponentId) :: Nil
    case BulletedList(formComponentId)             => FormCtx(formComponentId) :: Nil
    case StringOps(expr, _)                        => expr.leafs()
    case Concat(exprs)                             => exprs.flatMap(_.leafs())
    case CountryOfItmpAddress                      => this :: Nil
    case ChoicesRevealedField(formComponentId)     => FormCtx(formComponentId) :: Nil
    case ChoicesSelected(formComponentId)          => FormCtx(formComponentId) :: Nil
    case ChoicesAvailable(formComponentId)         => FormCtx(formComponentId) :: Nil
    case CountSelectedChoices(formComponentId)     => FormCtx(formComponentId) :: Nil
    case TaskStatus(_)                             => this :: Nil
    case LookupOps(expr, _)                        => expr.leafs()
  }

  def allFormComponentIds(): List[FormComponentId] =
    this.leafs().collect { case FormCtx(formComponentId) => formComponentId }
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
final case class PeriodValue(value: String) extends Expr
final case class LinkCtx(link: InternalLink) extends Expr
final case object Value extends Expr
final case class FormTemplateCtx(value: FormTemplateProp) extends Expr
final case class DateCtx(value: DateExpr) extends Expr
final case class DateFunction(value: DateProjection) extends Expr
final case class AddressLens(formComponentId: FormComponentId, detail: AddressDetail) extends Expr
final case class Period(dateCtx1: Expr, dateCtx2: Expr) extends Expr
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
final case class IndexOfDataRetrieveCtx(ctx: DataRetrieveCtx, index: Int) extends Expr
final case class NumberedList(formComponentId: FormComponentId) extends Expr
final case class BulletedList(formComponentId: FormComponentId) extends Expr
final case class StringOps(field1: Expr, stringFnc: StringFnc) extends Expr
final case class Concat(exprs: List[Expr]) extends Expr
final case object CountryOfItmpAddress extends Expr
final case class ChoicesRevealedField(formComponentId: FormComponentId) extends Expr
final case class ChoicesSelected(formComponentId: FormComponentId) extends Expr
final case class ChoicesAvailable(formComponentId: FormComponentId) extends Expr
final case class CountSelectedChoices(formComponentId: FormComponentId) extends Expr
final case class TaskStatus(taskId: TaskId) extends Expr

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

sealed trait StringFnc
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

final case class PeriodExt(period: Expr, func: PeriodFn) extends Expr

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
