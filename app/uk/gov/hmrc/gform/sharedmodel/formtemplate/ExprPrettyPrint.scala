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

object ExprPrettyPrint {

  def prettyPrintBooleanExpr(booleanExpr: BooleanExpr): String = booleanExpr match {
    case Equals(left, right)              => left.prettyPrint + " = " + right.prettyPrint
    case GreaterThan(left, right)         => left.prettyPrint + " > " + right.prettyPrint
    case GreaterThanOrEquals(left, right) => left.prettyPrint + " >= " + right.prettyPrint
    case LessThan(left, right)            => left.prettyPrint + " < " + right.prettyPrint
    case LessThanOrEquals(left, right)    => left.prettyPrint + " <= " + right.prettyPrint
    case DateBefore(left, right)          => prettyPrintDateExpr(left) + " is before " + prettyPrintDateExpr(right)
    case DateAfter(left, right)           => prettyPrintDateExpr(left) + " is after " + prettyPrintDateExpr(right)
    case Not(e)                           => "not(" + e.prettyPrint + ")"
    case Or(left, right)                  => "(" + left.prettyPrint + " or " + right.prettyPrint + ")"
    case And(left, right)                 => "(" + left.prettyPrint + " and " + right.prettyPrint + ")"
    case IsTrue                           => "true"
    case IsFalse                          => "false"
    case Contains(multiValueField, value) => multiValueField.prettyPrint + " contains " + value.prettyPrint
    case In(formCtx, _)                   => "In"
    case MatchRegex(expr, _)              => "Regex"
    case FormPhase(_)                     => "Form phase"
    case First(formCtx)                   => "First"
    case IsLogin(loginInfo)               => prettyPrintLoginInfo(loginInfo)
  }

  def prettyPrintLoginInfo(loginInfo: LoginInfo): String = loginInfo match {
    case LoginInfo.EmailLogin => "is logged in with email"
    case LoginInfo.GGLogin    => "is logged in with Goverment gateway"
  }

  def prettyPrintExpr(expr: Expr): String = expr match {
    case Add(field1: Expr, field2: Expr)         => field1.prettyPrint + " + " + field2.prettyPrint
    case Multiply(field1: Expr, field2: Expr)    => field1.prettyPrint + " * " + field2.prettyPrint
    case Subtraction(field1: Expr, field2: Expr) => field1.prettyPrint + " - " + field2.prettyPrint
    case Divide(field1: Expr, field2: Expr)      => field1.prettyPrint + " / " + field2.prettyPrint
    case HideZeroDecimals(field1: Expr)          => "hideZeroDecimals(" + field1.prettyPrint + ")"
    case IfElse(cond, field1: Expr, field2: Expr) =>
      "if(" + cond.prettyPrint + ") then " + field1.prettyPrint + " else " + field2.prettyPrint
    case Else(field1: Expr, field2: Expr)        => field1.prettyPrint + " else " + field2.prettyPrint
    case FormCtx(fcId)                           => fcId.value
    case Sum(field1: Expr)                       => "sum(" + field1.prettyPrint + ")"
    case Count(formComponentId: FormComponentId) => "count(" + formComponentId.value + ")"
    case AuthCtx(authInfo)                       => ExprPrettyPrint.prettyPrintAuthInfo(authInfo)
    case UserCtx(userField)                      => ExprPrettyPrint.prettyPrintUserInfo(userField)
    case Constant(constant)                      => if (constant.isEmpty) "<empty>" else constant
    case PeriodValue(period)                     => "Period: " + period
    case Value                                   => "Value"
    case FormTemplateCtx(_)                      => "Template"
    case ParamCtx(queryParam)                    => "Query param: " + queryParam.value
    case LinkCtx(_)                              => "Link"
    case LangCtx                                 => "Lang"
    case DateCtx(dateExpr)                       => ExprPrettyPrint.prettyPrintDateExpr(dateExpr)
    case DateFunction(dateProjection)            => ExprPrettyPrint.prettyPrintDateFunction(dateProjection)
    case Period(dateCtx1, dateCtx2)              => "period(" + dateCtx1.prettyPrint + ", " + dateCtx2.prettyPrint + ")"
    case PeriodExt(period, func)                 => period.prettyPrint
    case AddressLens(fcId, addressDetail) =>
      ExprPrettyPrint.prettyPrintAddressDetail(addressDetail) + " of " + fcId.value
    case DataRetrieveCtx(_, _)         => "Data retrieve"
    case DataRetrieveCount(_)          => "Data retrieve count"
    case CsvCountryCheck(_, _)         => "Country check"
    case CsvOverseasCountryCheck(_, _) => "Overseas country check"
    case CsvCountryCountCheck(_, _, _) => "Country count"
    case Size(_, _)                    => "Size"
    case Typed(e, _)                   => "Typed"
    case IndexOf(_, _)                 => "IndexOf"
    case IndexOfDataRetrieveCtx(_, _)  => "IndexOfDataRetrieveCtx"
    case NumberedList(_)               => "Numbered List"
    case BulletedList(_)               => "Bulleted List"
    case StringOps(_, stringFnc)       => stringFnc.toString
    case Concat(_)                     => "Concat"
    case CountryOfItmpAddress          => "CountryOfItmpAddress"
    case ChoicesRevealedField(_)       => "ChoicesRevealedField"
    case ChoiceLabel(_)                => "ChoiceLabel"
  }

  def prettyPrintItmpNameFocus(focus: ItmpNameFocus): String = focus match {
    case ItmpNameFocus.GivenName  => "given name"
    case ItmpNameFocus.MiddleName => "middle name"
    case ItmpNameFocus.FamilyName => "family name"
  }

  def prettyPrintAuthInfo(authInfo: AuthInfo): String = authInfo match {
    case AuthInfo.GG                     => "Goverment Gateway"
    case AuthInfo.PayeNino               => "Paye"
    case AuthInfo.EmailId                => "Email"
    case AuthInfo.SaUtr                  => "SA UTR"
    case AuthInfo.CtUtr                  => "CT UTR"
    case AuthInfo.EtmpRegistrationNumber => "Etmp registration number"
    case AuthInfo.Name                   => "Name"
    case AuthInfo.ItmpName               => "Itmp name"
    case AuthInfo.ItmpNameLens(focus)    => s"Itmp name ${prettyPrintItmpNameFocus(focus)}"
    case AuthInfo.ItmpDateOfBirth        => "Itmp date of birth"
    case AuthInfo.ItmpAddress            => "Itmp address"
    case AuthInfo.PayeRef                => "Paye Ref"
    case AuthInfo.Vrn                    => "Vrn"
  }

  def prettyPrintUserInfo(userField: UserField): String = userField match {
    case UserField.AffinityGroup      => "Affinity group"
    case UserField.Enrolment(_, _, _) => "enrolment"
    case UserField.EnrolledIdentifier => "Enrolled identifier"
    case UserField.CredentialRole     => "Credential role"
  }

  def prettyPrintDateFunction(dateProjection: DateProjection): String = dateProjection match {
    case DateProjection.Day(dateExpr)   => prettyPrintDateExpr(dateExpr)
    case DateProjection.Month(dateExpr) => prettyPrintDateExpr(dateExpr)
    case DateProjection.Year(dateExpr)  => prettyPrintDateExpr(dateExpr)
  }

  def prettyPrintDateExpr(dateExpr: DateExpr): String = dateExpr match {
    case DateValueExpr(dateExprValue)                           => prettyPrintDateExprValue(dateExprValue)
    case DateFormCtxVar(FormCtx(formComponentId))               => "date from " + formComponentId.value + " component"
    case DateExprWithOffset(dExpr: DateExpr, offset: OffsetYMD) => prettyPrintDateExpr(dExpr) + "offset"
    case HmrcTaxPeriodCtx(formCtx: FormCtx, hmrcTaxPeriodInfo: HmrcTaxPeriodInfo) =>
      "Hmrc tacx period" + formCtx.formComponentId.value
    case DateIfElse(ifElse: BooleanExpr, field1: DateExpr, field2: DateExpr) => "???"
    case DateOrElse(field1: DateExpr, field2: DateExpr)                      => "???"
  }

  def prettyPrintDateExprValue(dateExprValue: DateExprValue): String = dateExprValue match {
    case TodayDateExprValue                                  => "today"
    case ExactDateExprValue(year: Int, month: Int, day: Int) => s"$day/$month/$year"
  }

  def prettyPrintAddressDetail(addressDetail: AddressDetail): String = addressDetail match {
    case AddressDetail.Line1    => "line1"
    case AddressDetail.Line2    => "line2"
    case AddressDetail.Line3    => "line3"
    case AddressDetail.Line4    => "line4"
    case AddressDetail.Postcode => "postcode"
    case AddressDetail.Country  => "country"

  }
}
