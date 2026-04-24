/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.recalculation

import cats.syntax.all._
import java.time.LocalDate
import play.api.i18n.Messages
import scala.util.Try
import uk.gov.hmrc.gform.commons.{ BigDecimalUtil, NumberSetScale }
import uk.gov.hmrc.gform.eval.{ DataRetrieveEval, DateExprEval, ExprType, ExpressionResultWithTypeInfo, StaticTypeData }
import uk.gov.hmrc.gform.gform.AuthContextPrepop
import uk.gov.hmrc.gform.graph.processor.UserCtxEvaluatorProcessor
import uk.gov.hmrc.gform.lookup.{ LocalisedLookupOptions, LookupLabel, LookupOptions }
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.models.{ Atom, OptionDataUtils }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveId, DataRetrieveResult, ObligationDetail, SmartString, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.views.summary.TextFormatter
import uk.gov.hmrc.play.bootstrap.binders.RedirectUrl

trait Calculator {
  def evalBooleanExpr(booleanExpr: BooleanExpr): Boolean
  def evalBooleanExprList(booleanExpr: BooleanExpr, behaviour: Behaviour): List[Boolean]
  def evalExpr(expr: Expr): ExpressionResultWithTypeInfo
  def evalExpr(expr: Expr, staticTypeData: StaticTypeData, behaviour: Behaviour): EvaluationStatus
  def allModelComponentIds(modelComponentId: ModelComponentId): List[(ModelComponentId, EvaluationStatus)]
}

final class RealCalculator(
  metadata: Metadata,
  evaluationContext: EvaluationContext,
  formModelMetadata: FormModelMetadata,
  dataBridge: DataBridge
)(implicit messages: Messages)
    extends Calculator {

  override def allModelComponentIds(modelComponentId: ModelComponentId): List[(ModelComponentId, EvaluationStatus)] =
    dataBridge.allModelComponentIds(modelComponentId)

  override def evalBooleanExpr(booleanExpr: BooleanExpr): Boolean = {
    val staticTypeData = metadata.booleanExprStaticType(booleanExpr)

    evalBooleanExpr(booleanExpr, staticTypeData, Behaviour.Default).exists(_ == true)
  }

  override def evalBooleanExprList(booleanExpr: BooleanExpr, behaviour: Behaviour): List[Boolean] = {
    val staticTypeData = metadata.booleanExprStaticType(booleanExpr)

    evalBooleanExpr(booleanExpr, staticTypeData, behaviour)
  }

  private def earliestOf(evaluationStatuses: List[EvaluationStatus]): EvaluationStatus =
    findExtreme(evaluationStatuses, _.min)

  private def latestOf(evaluationStatuses: List[EvaluationStatus]): EvaluationStatus =
    findExtreme(evaluationStatuses, _.max)

  private def findExtreme(
    evaluationStatuses: List[EvaluationStatus],
    f: List[LocalDate] => LocalDate
  ): EvaluationStatus = {
    val dateResults = evaluationStatuses
      .flatMap {
        case EvaluationStatus.ListResult(xs) => xs
        case other                           => List(other)
      }
      .collect { case d: EvaluationStatus.DateResult =>
        d
      }
    if (dateResults.isEmpty) {
      EvaluationStatus.Empty
    } else {
      val localDates = dateResults.map(_.value)
      val localDate = f(localDates)
      if (dateResults.forall(_.flag.isDate)) {
        EvaluationStatus.DateResult.mkDate(localDate)
      } else if (dateResults.forall(_.flag.isCalendarDate)) {
        EvaluationStatus.DateResult.mkCalendarDate(localDate)
      } else if (dateResults.forall(_.flag.isTaxPeriodDate)) {
        EvaluationStatus.DateResult.mkTaxPeriodDate(localDate)
      } else {
        // We have a mix of date result flags. Let use first flag as determinator
        val flag = dateResults.head.flag
        if (flag.isDate) {
          EvaluationStatus.DateResult.mkDate(localDate)
        } else if (flag.isCalendarDate) {
          EvaluationStatus.DateResult.mkCalendarDate(localDate)
        } else {
          EvaluationStatus.DateResult.mkTaxPeriodDate(localDate)
        }
      }
    }
  }

  private def evalIfElse(
    cond: BooleanExpr,
    expr1: Expr,
    expr2: Expr,
    staticTypeData: StaticTypeData,
    behaviour: Behaviour
  ): EvaluationStatus =
    evalBooleanExprList(cond, behaviour) match {
      case cond :: Nil =>
        if (cond)
          evalExpr(expr1, staticTypeData, behaviour)
        else
          evalExpr(expr2, staticTypeData, behaviour)
      case conds =>
        EvaluationStatus.ListResult(conds.zipWithIndex.map { case (cond, index) =>
          val expr = if (cond) expr1 else expr2

          evalExpr(expr, staticTypeData, behaviour) match {
            case EvaluationStatus.ListResult(list) => list(index)
            case otherwise                         => otherwise
          }
        })
    }

  private def evalIsLogin(loginInfo: LoginInfo): Boolean =
    loginInfo match {
      case LoginInfo.GGLogin    => evaluationContext.retrievals.maybeGovermentGatewayId.isDefined
      case LoginInfo.EmailLogin => evaluationContext.retrievals.maybeEmailId.isDefined
      case _                    => false
    }

  private def evalBetween(
    evaluationStatus1: EvaluationStatus,
    evaluationStatus2: EvaluationStatus,
    measurementType: MeasurementType
  ): EvaluationStatus =
    (evaluationStatus1, evaluationStatus2) match {
      case (d1: EvaluationStatus.DateResult, d2: EvaluationStatus.DateResult) => betweenDates(d1, d2, measurementType)

      case (EvaluationStatus.ListResult(list1), EvaluationStatus.ListResult(list2)) =>
        EvaluationStatus.ListResult(list1.zip(list2).map {
          case (d1: EvaluationStatus.DateResult, d2: EvaluationStatus.DateResult) =>
            betweenDates(d1, d2, measurementType)
          case _ => EvaluationStatus.Empty
        })
      case _ => EvaluationStatus.Empty
    }

  private def betweenDates(
    dateResult1: EvaluationStatus.DateResult,
    dateResult2: EvaluationStatus.DateResult,
    measurementType: MeasurementType
  ): EvaluationStatus.NumberResult = {
    val d1 = dateResult1.value
    val d2 = dateResult2.value
    measurementType match {
      case MeasurementType.Days =>
        EvaluationStatus.NumberResult(java.time.temporal.ChronoUnit.DAYS.between(d1, d2) + 1)
      case MeasurementType.Weeks =>
        EvaluationStatus.NumberResult((java.time.temporal.ChronoUnit.DAYS.between(d1, d2) + 1) / 7)
    }
  }

  private def evalCountryOfItmpAddress(): EvaluationStatus = {
    val itmpRetrievals = evaluationContext.thirdPartyData.itmpRetrievals
    itmpRetrievals.flatMap(_.itmpAddress).flatMap(_.countryName) match {
      case None          => EvaluationStatus.Empty
      case Some(country) => EvaluationStatus.StringResult(country)
    }
  }

  private def evalLang(): EvaluationStatus = EvaluationStatus.StringResult(evaluationContext.lang.langADTToString)

  private def evalFormTemplateProp(value: FormTemplateProp): EvaluationStatus =
    value match {
      case FormTemplateProp.Id => EvaluationStatus.StringResult(evaluationContext.formTemplateId.value)
      case FormTemplateProp.SubmissionReference =>
        val seed =
          evaluationContext.customSubmissionRef.fold(evaluationContext.envelopeId.value)(customSubmissionReference =>
            evalExpr(customSubmissionReference.expr).evaluationStatus match {
              case EvaluationStatus.StringResult(value) => value
              case _                                    => ""

            }
          )
        EvaluationStatus.StringResult(SubmissionRef.fromSeed(seed).value)
      case FormTemplateProp.FileSizeLimit => EvaluationStatus.NumberResult(evaluationContext.fileSizeLimit.value)
    }

  private def evalFormPhase(value: FormPhaseValue): Boolean =
    evaluationContext.formPhase.fold(false)(_.value == value)

  private def evalQueryParam(queryParam: QueryParam): EvaluationStatus =
    EvaluationStatus.StringResult(evaluationContext.thirdPartyData.queryParams(queryParam))

  private def evalLink(
    internalLink: InternalLink,
    formModelMetadata: FormModelMetadata
  ): EvaluationStatus = {
    val link =
      internalLink match {
        case InternalLink.PrintSummaryPdf =>
          uk.gov.hmrc.gform.gform.routes.SummaryController
            .downloadPDF(evaluationContext.formTemplateId, evaluationContext.maybeAccessCode)
            .url
        case InternalLink.PrintAcknowledgementPdf =>
          uk.gov.hmrc.gform.gform.routes.AcknowledgementController
            .downloadPDF(evaluationContext.maybeAccessCode, evaluationContext.formTemplateId)
            .url
        case InternalLink.NewFormForTemplate(formTemplateId) =>
          uk.gov.hmrc.gform.gform.routes.NewFormController
            .dashboardClean(formTemplateId)
            .url
        case InternalLink.NewForm =>
          uk.gov.hmrc.gform.gform.routes.NewFormController
            .dashboard(evaluationContext.formTemplateId)
            .url
        case InternalLink.NewSession =>
          uk.gov.hmrc.gform.gform.routes.NewFormController
            .dashboardWithNewSession(evaluationContext.formTemplateId)
            .url
        case InternalLink.SignOut =>
          uk.gov.hmrc.gform.gform.routes.SignOutController
            .signOut(evaluationContext.formTemplateId)
            .url
        case InternalLink.PageLink(id) =>
          computePageLink(id, evaluationContext, formModelMetadata)
        case InternalLink.Download(fileName) =>
          uk.gov.hmrc.gform.gform.routes.DownloadController
            .downloadFile(fileName)
            .url
        case InternalLink.Image(fileName) =>
          uk.gov.hmrc.gform.gform.routes.ImageController
            .image(
              evaluationContext.formTemplateId,
              evaluationContext.maybeAccessCode,
              fileName
            )
            .url
        case InternalLink.UrlLink(url) =>
          uk.gov.hmrc.gform.gform.routes.RedirectController.redirect(RedirectUrl(url)).url
        case InternalLink.PrintSectionPdf =>
          uk.gov.hmrc.gform.gform.routes.PrintSectionController
            .downloadNotificationPDF(evaluationContext.formTemplateId, evaluationContext.maybeAccessCode)
            .url
        case InternalLink.SummaryPage =>
          uk.gov.hmrc.gform.gform.routes.SummaryController
            .summaryById(evaluationContext.formTemplateId, evaluationContext.maybeAccessCode, None, None)
            .url

      }
    EvaluationStatus.StringResult(link)
  }

  private def computePageLink(
    forPageId: PageId,
    evaluationContext: EvaluationContext,
    formModelMetadata: FormModelMetadata
  ) = {
    val forModelPageId = forPageId.modelPageId
    formModelMetadata.pageIdSectionNumberMap.get(forModelPageId) match {
      case Some(sectionNumber) =>
        uk.gov.hmrc.gform.gform.routes.FormController
          .formSection(evaluationContext.formTemplateId, evaluationContext.maybeAccessCode, sectionNumber)
          .url
      case None =>
        formModelMetadata.pageIdSectionNumberMap.toList
          .sortBy(_._1.maybeIndex)(Ordering[Option[Int]].reverse)
          .find { case (modelPageId, _) =>
            modelPageId.baseId == forModelPageId.baseId
          }
          .fold("") { case (_, sectionNumber) =>
            uk.gov.hmrc.gform.gform.routes.FormController
              .formSection(evaluationContext.formTemplateId, evaluationContext.maybeAccessCode, sectionNumber)
              .url
          }
    }
  }

  private def getLookupLabelById(
    evaluationStatus: EvaluationStatus,
    lookupFnc: LookupFnc,
    evaluationContext: EvaluationContext,
    staticTypeData: StaticTypeData,
    messages: Messages
  ): EvaluationStatus = evaluationStatus match {
    case lr: EvaluationStatus.ListResult =>
      lr.map { evaluationStatus =>
        val value = evaluationStatus.stringRepresentation(staticTypeData, messages)
        evalLookupFnc(lookupFnc, value, evaluationContext)
      }
    case evaluationStatus =>
      val value = evaluationStatus.stringRepresentation(staticTypeData, messages)
      evalLookupFnc(lookupFnc, value, evaluationContext)
  }

  private def evalLookupFnc(
    lookupFnc: LookupFnc,
    value: String,
    evaluationContext: EvaluationContext
  ): EvaluationStatus =
    lookupFnc match {
      case LookupFnc.CountryName    => getLookupLabelById(value, Register.Country, evaluationContext)
      case LookupFnc.SicDescription => getLookupLabelById(value, Register.SicCode, evaluationContext)
    }

  def evalAddressLens(evaluationStatus: EvaluationStatus, details: AddressDetail): EvaluationStatus = {
    // We don't know if addressResult is overseas address or uk address, so we try both.
    val atoms = Set(details.toAddressAtom, details.toOverseasAddressAtom)

    evaluationStatus match {
      case EvaluationStatus.AddressResult(address) =>
        val maybeAddressLine: Option[(Atom, String)] = address.find { case (atom, value) => atoms(atom) }
        maybeAddressLine.fold[EvaluationStatus](EvaluationStatus.Empty) { case (_, value) =>
          EvaluationStatus.StringResult(value)
        }
      case otherwise => otherwise
    }
  }

  def evalLookupColumn(
    evaluationStatus: EvaluationStatus,
    lookupColumn: LookupColumn,
    evaluationContext: EvaluationContext,
    staticTypeData: StaticTypeData,
    messages: Messages
  ): EvaluationStatus =
    evaluationStatus match {
      case lr: EvaluationStatus.ListResult =>
        lr.map { evaluationStatus =>
          val value = evaluationStatus.stringRepresentation(staticTypeData, messages)
          evalLookupColumn(lookupColumn, value, evaluationContext)
        }

      case EvaluationStatus.AddressResult(address) =>
        val addressEvaluationStatus = evalAddressLens(evaluationStatus, AddressDetail.Country)
        val value = addressEvaluationStatus.stringRepresentation(staticTypeData, messages)
        evalLookupColumn(lookupColumn, value, evaluationContext)

      case evaluationStatus =>
        val value = evaluationStatus.stringRepresentation(staticTypeData, messages)
        evalLookupColumn(lookupColumn, value, evaluationContext)
    }

  def evalDataRetrieveCtx(
    dataRetrieveCtx: DataRetrieveCtx
  ): EvaluationStatus =
    evaluationContext.thirdPartyData.dataRetrieve.fold[EvaluationStatus](EvaluationStatus.Empty) { dr =>
      dataRetrieveCtx.attribute.name match {
        case "registeredOfficeAddress"                   => handleAddr(dataRetrieveCtx, dr)
        case "primaryAddress"                            => handleAddr(dataRetrieveCtx, dr)
        case "agencyAddress"                             => handleAddr(dataRetrieveCtx, dr)
        case DataRetrieve.failureCountAttribute.name     => DataRetrieveEval.getFailureCount(dr, dataRetrieveCtx)
        case DataRetrieve.failureIsBlockedAttribute.name => DataRetrieveEval.getIsBlocked(dr, dataRetrieveCtx)
        case DataRetrieve.failureResetTimeAttribute.name => DataRetrieveEval.getFailureResetTime(dr, dataRetrieveCtx)
        case DataRetrieve.failureResetDateAttribute.name => DataRetrieveEval.getFailureResetDate(dr, dataRetrieveCtx)
        case _ =>
          DataRetrieveEval.getDataRetrieveAttribute(dr, dataRetrieveCtx) match {
            case Some(value :: Nil) =>
              val isNumber =
                metadata.dataRetrieveAll.isNumber(dataRetrieveCtx.id.modelPageId.baseId, dataRetrieveCtx.attribute)
              if (isNumber) {
                BigDecimalUtil
                  .toBigDecimalSafe(value)
                  .fold[EvaluationStatus](
                    EvaluationStatus.Invalid(s"Number - cannot convert '$value' to number")
                  )(value => EvaluationStatus.NumberResult(value))
              } else EvaluationStatus.StringResult(value)
            case Some(xs) => EvaluationStatus.ListResult(xs.map(EvaluationStatus.StringResult))
            case None     => EvaluationStatus.Empty
          }

      }
    }

  private def handleAddr(
    dataRetrieveCtx: DataRetrieveCtx,
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult]
  ): EvaluationStatus =
    DataRetrieveEval.getDataRetrieveAddressAttribute(dataRetrieve, dataRetrieveCtx)

  private def evalUserField(userField: UserField): EvaluationStatus = {
    val value = UserCtxEvaluatorProcessor
      .processEvaluation(evaluationContext.retrievals, userField, evaluationContext.authConfig)
    EvaluationStatus.StringResult(value)
  }
  private def evalAuthInfo(authInfo: AuthInfo)(implicit
    messages: Messages
  ): EvaluationStatus =
    AuthContextPrepop.values(
      authInfo,
      evaluationContext.retrievals,
      evaluationContext.thirdPartyData.itmpRetrievals
    )

  private def evalCount(
    metadata: Metadata,
    baseComponentId: BaseComponentId,
    statuses: List[EvaluationStatus]
  ): EvaluationStatus = {
    val size = if (metadata.addToListIds(baseComponentId)) {
      if (statuses.contains(EvaluationStatus.Hidden)) {
        0
      } else {
        statuses.filter {
          case EvaluationStatus.OptionResult(values) => values.toList === List("0")
          case _                                     => false
        }.size + 1
      }
    } else {
      statuses.filter {
        case EvaluationStatus.Hidden => false
        case _                       => true
      }.size
    }

    EvaluationStatus.NumberResult(size)

  }

  private val ChoiceIndexedValue = "^(.+)_(\\d+)$".r
  private def evalIndexOfInChoice(
    evaluationStatus: EvaluationStatus,
    stringValue: String
  ): EvaluationStatus =
    evaluationStatus match {
      case EvaluationStatus.OptionResult(values) =>
        values.toList match {
          case value :: Nil =>
            value match {
              case ChoiceIndexedValue(optionValue, index) =>
                if (optionValue === stringValue)
                  EvaluationStatus.NumberResult(index.toInt)
                else
                  EvaluationStatus.Empty
              case _ => EvaluationStatus.Empty
            }
          case _ => EvaluationStatus.Empty
        }
      case _ => EvaluationStatus.Empty
    }

  private def evalPeriod(l: EvaluationStatus, r: EvaluationStatus, func: PeriodFn): EvaluationStatus =
    (l, r) match {
      case (EvaluationStatus.DateResult(value1, _), EvaluationStatus.DateResult(value2, _)) =>
        val p = java.time.Period.between(value1, value2)
        func match {
          case PeriodFn.Identity    => EvaluationStatus.PeriodResult(p)
          case PeriodFn.Sum         => EvaluationStatus.PeriodResult(p)
          case PeriodFn.TotalMonths => EvaluationStatus.NumberResult(p.toTotalMonths)
          case PeriodFn.Years       => EvaluationStatus.NumberResult(p.getYears)
          case PeriodFn.Months      => EvaluationStatus.NumberResult(p.getMonths)
          case PeriodFn.Days        => EvaluationStatus.NumberResult(p.getDays)
        }
      case (EvaluationStatus.ListResult(xs1), EvaluationStatus.ListResult(xs2)) =>
        val periods: List[java.time.Period] =
          xs1.zip(xs2).collect { case (EvaluationStatus.DateResult(ld1, _), EvaluationStatus.DateResult(ld2, _)) =>
            java.time.Period.between(ld1, ld2)
          }

        val p: java.time.Period = periods.foldLeft(java.time.Period.ZERO)(_.plus(_)).normalized()

        if (periods.isEmpty) {
          EvaluationStatus.Empty
        } else {
          func match {
            case PeriodFn.Identity    => EvaluationStatus.StringResult(periods.map(_.toString).mkString(", "))
            case PeriodFn.Sum         => EvaluationStatus.PeriodResult(p)
            case PeriodFn.TotalMonths => EvaluationStatus.NumberResult(p.toTotalMonths)
            case PeriodFn.Years       => EvaluationStatus.NumberResult(p.getYears)
            case PeriodFn.Months      => EvaluationStatus.NumberResult(p.getMonths)
            case PeriodFn.Days        => EvaluationStatus.NumberResult(p.getDays)
          }
        }
      case _ => EvaluationStatus.Empty
    }

  private def countDataRetrieve(
    dataRetrieveId: DataRetrieveId
  ): EvaluationStatus =
    evaluationContext.thirdPartyData.dataRetrieve.fold[EvaluationStatus](EvaluationStatus.Empty) { dr =>
      dr.get(dataRetrieveId).fold[EvaluationStatus](EvaluationStatus.Empty) { dataRetrieveResult =>
        EvaluationStatus.NumberResult(dataRetrieveResult.data.size)
      }
    }

  private def evalLookupColumn(
    lookupColumn: LookupColumn,
    value: String,
    evaluationContext: EvaluationContext
  ): EvaluationStatus =
    evaluationContext.lookupRegister
      .get(lookupColumn.formComponentId.baseComponentId)
      .map { register =>
        val lookupOptions: LocalisedLookupOptions = evaluationContext.lookupRegistry.getLookupOptions(register)
        evalLookup(value, lookupColumn.column, lookupOptions)
      }
      .getOrElse(EvaluationStatus.Empty)

  private def evalLookup(
    value: String,
    column: String,
    lookupOptions: LocalisedLookupOptions
  ): EvaluationStatus =
    lookupOptions.fold[EvaluationStatus](EvaluationStatus.Empty)(
      _.get(LookupLabel(value))
        .flatMap(li => LookupOptions.getLookupValue(li, column))
        .map(EvaluationStatus.StringResult(_))
        .getOrElse(EvaluationStatus.Empty)
    )(evaluationContext.lang)

  private def getLookupLabelById(
    value: String,
    register: Register,
    evaluationContext: EvaluationContext
  ): EvaluationStatus = {

    val lookupOptions: LocalisedLookupOptions = evaluationContext.lookupRegistry.getLookupOptions(register)

    lookupOptions.fold[EvaluationStatus](EvaluationStatus.Empty) { options =>
      options.options
        .collectFirst { case (l, lo) if lo.id.id === value => EvaluationStatus.StringResult(l.label) }
        .getOrElse(EvaluationStatus.Empty)
    }(evaluationContext.lang)
  }

  private def evalTyped(er: EvaluationStatus, tpe: ExplicitExprType): EvaluationStatus =
    tpe match {
      case ExplicitExprType.Sterling(roundingMode) =>
        er.withNumberResult(bigDecimal => NumberSetScale.setScale(bigDecimal, 2, roundingMode))
      case ExplicitExprType.WholeSterling(roundingMode) =>
        er.withNumberResult(bigDecimal => NumberSetScale.setScale(bigDecimal, 0, roundingMode))
      case ExplicitExprType.Number(fractionalDigits, roundingMode) =>
        er.withNumberResult(bigDecimal => NumberSetScale.setScale(bigDecimal, fractionalDigits, roundingMode))
      case ExplicitExprType.Text => er
    }

  private def evalIndexOfDataRetrieveCtx(
    dataRetrieveCtx: DataRetrieveCtx,
    expr: Expr,
    staticTypeData: StaticTypeData,
    behaviour: Behaviour
  ): EvaluationStatus =
    evalExpr(expr, StaticTypeData(ExprType.Number, None), behaviour) match {
      case EvaluationStatus.NumberResult(index) =>
        evalExpr(dataRetrieveCtx, staticTypeData, behaviour) match {
          case EvaluationStatus.ListResult(statuses) =>
            Try(statuses(index.toInt)).getOrElse(EvaluationStatus.Empty)
          case otherwise => otherwise
        }
      case otherwise => otherwise
    }

  private def isFirst(
    formCtx: FormCtx,
    staticTypeData: StaticTypeData,
    insideAtl: Boolean,
    behaviour: Behaviour
  ): List[Boolean] =
    if (insideAtl) {
      evalExpr(Index(formCtx.formComponentId), staticTypeData, behaviour) match {
        case EvaluationStatus.NumberResult(x) => (x.toInt == 1) :: Nil
        case _                                => false :: Nil
      }
    } else {
      evalExpr(FormCtx(formCtx.formComponentId), staticTypeData, behaviour) match {
        case EvaluationStatus.ListResult(h :: t) => true :: t.map(_ => false)
        case _                                   => false :: Nil
      }
    }

  private def evalIn(
    expr: Expr,
    dataSource: DataSource,
    staticTypeData: StaticTypeData,
    behaviour: Behaviour
  )(implicit
    messages: Messages
  ): List[Boolean] = {
    val statuses = evalExpr(expr, staticTypeData, behaviour) match {
      case EvaluationStatus.ListResult(list) => list
      case evaluationStatus                  => List(evaluationStatus)
    }

    statuses.map { evaluationStatus =>
      val value = evaluationStatus.stringRepresentation(staticTypeData, messages)
      evaluationContext.thirdPartyData.booleanExprCache.get(dataSource, value).getOrElse(false)
    }
  }

  private def evalLookupColumnCount(evaluationStatus: EvaluationStatus, value: String, staticTypeData: StaticTypeData)(
    implicit messages: Messages
  ): EvaluationStatus =
    evaluationStatus match {
      case EvaluationStatus.ListResult(list) =>
        val count = list.count(_.stringRepresentation(staticTypeData, messages) === value)
        EvaluationStatus.NumberResult(count)
      case _ => EvaluationStatus.Empty
    }

  private def evalHideZeroDecimals(evaluationStatus: EvaluationStatus, staticTypeData: StaticTypeData)(implicit
    messages: Messages
  ): EvaluationStatus = {
    val stringResult = evaluationStatus.stringRepresentation(staticTypeData, messages)

    val parts = stringResult.split("\\.")
    val striped =
      if (parts.length == 2 && parts(1).forall(_ == '0'))
        TextFormatter.stripDecimal(stringResult)
      else
        stringResult
    EvaluationStatus.StringResult(striped)
  }

  private def evalDuplicateExists(
    formCtxs: Seq[FormCtx],
    staticTypeData: StaticTypeData
  ): Boolean = {
    val rows: Seq[List[EvaluationStatus]] = formCtxs
      .map { formCtx =>
        evalExpr(formCtx, staticTypeData, Behaviour.LessThanCurrent)
      }
      .collect { case EvaluationStatus.ListResult(list) =>
        list
      }

    val min: Int = rows.map(_.size).min
    val alignedRows = rows.map(_.take(min))

    val columns: Seq[Seq[EvaluationStatus]] = alignedRows.transpose.filter { column =>
      column.forall {
        case EvaluationStatus.Hidden | EvaluationStatus.Empty => false
        case _                                                => true
      }
    }

    (columns.distinct.size =!= columns.size)
  }

  private def getChoicesSelected(evaluationStatus: EvaluationStatus): EvaluationStatus = {
    val count = evaluationStatus match {
      case EvaluationStatus.OptionResult(values) => values.size
      case EvaluationStatus.ListResult(statuses) =>
        statuses.collect {
          case EvaluationStatus.OptionResult(values) => values.size
          case _                                     => 0
        }.sum
      case _ => 0
    }

    EvaluationStatus.NumberResult(count)
  }

  private def getChoicesAvailable(
    formComponentId: FormComponentId,
    metadata: Metadata,
    insideAtl: Boolean
  )(implicit
    messages: Messages
  ): EvaluationStatus = {
    val evaluationStatusInside: Int =
      if (insideAtl) {
        evalExpr(FormCtx(formComponentId)).evaluationStatus match {
          case EvaluationStatus.OptionResult(values) => values.size
          case _                                     => 0
        }
      } else 0
    val selected: Int = evalExpr(CountSelectedChoices(formComponentId)).evaluationStatus match {
      case EvaluationStatus.NumberResult(value) => value.toInt
      case _                                    => 0
    }
    val choicesCount =
      getChoicesTotalCount(formComponentId, metadata)
    choicesCount - EvaluationStatus.NumberResult(selected - evaluationStatusInside)
  }

  private def getChoicesTotalCount(
    formComponentId: FormComponentId,
    metadata: Metadata
  )(implicit
    messages: Messages
  ): EvaluationStatus = {
    val modelComponentId = formComponentId.modelComponentId
    val choicesAvailable = metadata.choiceLookup
      .get(modelComponentId.baseComponentId)
      .map { case (formComponent, choice) =>
        val optionsData: List[OptionData] = choice.options
          .filter(_.includeIf.fold(true)(iif => evalBooleanExpr(iif.booleanExpr)))
        if (choice.noDuplicates) {
          OptionDataUtils.expand(formComponent, choice, this) match {
            case IsChoice(choice) =>
              val labels: List[SmartString] = choice.options.map(_.label)

              val labelsAndExprs: List[String] = labels.map { label =>
                val rawLabel: String = label.rawValue(evalBooleanExpr)(evaluationContext.lang)
                val exprs: List[String] =
                  label
                    .interpolations(evalBooleanExpr)
                    .map(evalExpr)
                    .map(_.stringRepresentation(messages))
                rawLabel + "-" + exprs.mkString("-") // We don't need to apply interpolations precisely
              }

              labelsAndExprs.toSet.size

            case unexpected => throw new Exception(s"Expected Choice component, but got: $unexpected")
          }
        } else {
          optionsData.map {
            case _: OptionData.IndexBased                      => 1
            case OptionData.ValueBased(_, _, _, None, _, _, _) => 1
            case OptionData.ValueBased(_, _, _, Some(Dynamic.DataRetrieveBased(indexOfDataRetrieveCtx)), _, _, _) =>
              evaluationContext.thirdPartyData.dataRetrieve
                .flatMap(dr => dr.get(indexOfDataRetrieveCtx.ctx.id))
                .fold(0)(drr => drr.data.size)
            case OptionData.ValueBased(_, _, _, Some(Dynamic.ATLBased(fcId)), _, _, _) =>
              evalExpr(Count(fcId)).evaluationStatus match {
                case EvaluationStatus.NumberResult(c) => c.toInt
                case _                                => 0
              }
          }.sum
        }
      }
      .getOrElse(0)
    EvaluationStatus.NumberResult(choicesAvailable)
  }

  override def evalExpr(expr: Expr): ExpressionResultWithTypeInfo = {
    val staticTypeData = metadata.exprStaticType(expr)
    val evaluationStatus = evalExpr(expr, staticTypeData, Behaviour.Default)
    ExpressionResultWithTypeInfo(evaluationStatus, staticTypeData)
  }

  private def evalExpr(expr: Expr, behaviour: Behaviour): EvaluationStatus = {
    val staticTypeData = metadata.exprStaticType(expr)
    evalExpr(expr, staticTypeData, behaviour)
  }

  override def evalExpr(expr: Expr, staticTypeData: StaticTypeData, behaviour: Behaviour): EvaluationStatus =
    expr match {
      case Add(lhs, rhs) =>
        // When we are outside ATL, but rhs is referenceing ATL we need change behaviour on List producing.
        // TODO JoVl investigate ATL reference on the left, but not on the right
        val finalBehaviour = behaviour(metadata, rhs)
        val l = evalExpr(lhs, staticTypeData, finalBehaviour)
        val r = evalExpr(rhs, staticTypeData, finalBehaviour)
        l + r
      case Subtraction(lhs, rhs) =>
        val l = evalExpr(lhs, staticTypeData, behaviour)
        val r = evalExpr(rhs, staticTypeData, behaviour)
        l - r
      case Multiply(lhs, rhs) =>
        val l = evalExpr(lhs, staticTypeData, behaviour)
        val r = evalExpr(rhs, staticTypeData, behaviour)
        l * r
      case Divide(lhs, rhs) =>
        val l = evalExpr(lhs, staticTypeData, behaviour)
        val r = evalExpr(rhs, staticTypeData, behaviour)
        l / r
      case DateFunction(dateFunc) =>
        evalDateExpr(dateFunc.dateExpr, staticTypeData, behaviour).mapDateResult(localDate =>
          EvaluationStatus.NumberResult(dateFunc.toValue(localDate))
        )
      case FormCtx(fcId) => dataBridge.evalFormCtx(fcId, behaviour)
      case Constant(c) =>
        staticTypeData.exprType match {
          case ExprType.Number =>
            if (c.trim.isEmpty)
              EvaluationStatus.Empty
            else
              BigDecimalUtil.toBigDecimalOrString(c)
          case ExprType.String => EvaluationStatus.StringResult(c)
          case _               => EvaluationStatus.StringResult(c)
        }
      case IfElse(cond, expr1, expr2) =>
        evalIfElse(cond, expr1, expr2, staticTypeData, behaviour)
      case Sum(expr) =>
        evalExpr(expr, staticTypeData, Behaviour.LessThanCurrent) match {
          case EvaluationStatus.ListResult(statuses) =>
            statuses.foldLeft[EvaluationStatus](EvaluationStatus.Empty)(_ + _)
          case otherwise => otherwise
        }
      case DateCtx(dateExpr) => evalDateExpr(dateExpr, staticTypeData, behaviour)
      case d @ DataRetrieveCtx(dataRetrieveId, _) =>
        val drId = dataBridge.liftDataRetrieveId(dataRetrieveId)
        val dataRetrieveCtx = d.copy(id = drId)
        evalDataRetrieveCtx(dataRetrieveCtx)
      case UserCtx(userField) => evalUserField(userField)
      case AuthCtx(authInfo)  => evalAuthInfo(authInfo)
      case TaskStatus(taskId) =>
        EvaluationStatus.StringResult(
          evaluationContext.taskIdTaskStatus.mapping.get(taskId).map(_.asString).getOrElse("")
        )
      case Concat(exprs) =>
        val evaluationStatuses: List[EvaluationStatus] = exprs.map(expr => evalExpr(expr, staticTypeData, behaviour))
        EvaluationStatus.StringResult(
          evaluationStatuses.map(_.stringRepresentation(staticTypeData, messages)).mkString("")
        )
      case Count(fcId) =>
        evalExpr(FormCtx(fcId), staticTypeData, Behaviour.All) match {
          case EvaluationStatus.ListResult(statuses) => evalCount(metadata, fcId.baseComponentId, statuses)
          case otherwise                             => otherwise
        }
      case Size(fcId, sizeRefType) =>
        evalExpr(FormCtx(fcId), staticTypeData, Behaviour.LessThanCurrent) match {
          case EvaluationStatus.ListResult(statuses) =>
            val indexString = sizeRefType match {
              case SizeRefType.IndexBased(index) => index.toString
              case SizeRefType.ValueBased(value) => value
            }

            val size: Int =
              statuses.count(_.contains(EvaluationStatus.OptionResult(List(indexString)), behaviour).exists(identity))

            EvaluationStatus.NumberResult(size)

          case otherwise => otherwise
        }
      case Index(fcId) =>
        dataBridge
          .maybeIndex(fcId)
          .fold[EvaluationStatus](EvaluationStatus.Empty)(EvaluationStatus.NumberResult(_))
      case IndexOf(fcId, index) =>
        evalExpr(FormCtx(fcId), staticTypeData, behaviour) match {
          case EvaluationStatus.ListResult(statuses) =>
            Try(statuses(index)).getOrElse(EvaluationStatus.Empty)
          case _ => EvaluationStatus.Empty
        }
      case IndexOfInChoice(OptionDataValue.StringBased(stringValue), fcId) =>
        val evaluationStatus = evalExpr(FormCtx(fcId), staticTypeData, behaviour)
        evalIndexOfInChoice(evaluationStatus, stringValue)
      case IndexOfInChoice(OptionDataValue.ExprBased(_), fcId) => EvaluationStatus.Empty
      case StringOps(expr, stringFnc) =>
        val s = evalExpr(expr, staticTypeData, behaviour).stringRepresentation(staticTypeData, messages)
        EvaluationStatus.StringResult(stringFnc.eval(s))
      case PeriodValue(value) => EvaluationStatus.PeriodResult(java.time.Period.parse(value))
      case Period(lhs, rhs, func) =>
        val l = evalExpr(lhs, staticTypeData, behaviour)
        val r = evalExpr(rhs, staticTypeData, behaviour)
        evalPeriod(l, r, func)
      case DataRetrieveCount(dataRetrieveId) =>
        val drId = dataBridge.liftDataRetrieveId(dataRetrieveId)
        countDataRetrieve(drId)
      case lc: LookupColumn =>
        val expr = FormCtx(lc.formComponentId)
        val evaluationStatus = evalExpr(expr, staticTypeData, behaviour)
        evalLookupColumn(evaluationStatus, lc, evaluationContext, staticTypeData, messages)
      case LookupOps(expr, lookupFnc) =>
        val evaluationStatus = evalExpr(expr, staticTypeData, behaviour)
        getLookupLabelById(evaluationStatus, lookupFnc, evaluationContext, staticTypeData, messages)
      case Typed(expr, explicitExprType) =>
        evalTyped(evalExpr(expr, staticTypeData, behaviour), explicitExprType)
      case IndexOfDataRetrieveCtx(dataRetrieveCtx, expr) =>
        evalIndexOfDataRetrieveCtx(dataRetrieveCtx, expr, staticTypeData, behaviour)
      case Between(expr1, expr2, measurementType) =>
        val res1 = evalExpr(expr1, staticTypeData, behaviour)
        val res2 = evalExpr(expr2, staticTypeData, behaviour)
        evalBetween(res1, res2, measurementType)
      case CountryOfItmpAddress   => evalCountryOfItmpAddress()
      case LangCtx                => evalLang()
      case FormTemplateCtx(value) => evalFormTemplateProp(value)
      case LinkCtx(internalLink)  => evalLink(internalLink, formModelMetadata)
      case ParamCtx(queryParam)   => evalQueryParam(queryParam)
      case AddressLens(formComponentId, details) =>
        val evaluationStatus = evalExpr(FormCtx(formComponentId), staticTypeData, behaviour)
        evalAddressLens(evaluationStatus, details)
      case HideZeroDecimals(expr: Expr) =>
        val evaluationStatus = evalExpr(expr, staticTypeData, behaviour)
        evalHideZeroDecimals(evaluationStatus, staticTypeData)
      case CsvCountryCountCheck(fcId, column, value) =>
        val evaluationStatus = evalExpr(LookupColumn(fcId, column), staticTypeData, behaviour)
        evalLookupColumnCount(evaluationStatus, value, staticTypeData)
      case ChoicesSelected(formComponentId) =>
        getChoicesSelected(evalExpr(FormCtx(formComponentId), staticTypeData, behaviour))
      case CountSelectedChoices(formComponentId) =>
        getChoicesSelected(evalExpr(FormCtx(formComponentId), staticTypeData, Behaviour.All))
      case ChoicesCount(formComponentId) => getChoicesTotalCount(formComponentId, metadata)
      case ChoicesAvailable(formComponentId, insideAtl) =>
        getChoicesAvailable(formComponentId, metadata, insideAtl.getOrElse(false))
      case BulletedList(formComponentId) => evalExpr(FormCtx(formComponentId), staticTypeData, behaviour)
      case NumberedList(formComponentId) => evalExpr(FormCtx(formComponentId), staticTypeData, behaviour)
      case BulletedListChoicesSelected(formComponentId, _) =>
        evalExpr(FormCtx(formComponentId), staticTypeData, behaviour)
      case NumberedListChoicesSelected(formComponentId, _) =>
        evalExpr(FormCtx(formComponentId), staticTypeData, behaviour)
      case ChoicesRevealedField(formComponentId) => evalExpr(FormCtx(formComponentId), staticTypeData, behaviour)
      case DisplayAsEntered(formComponentId)     => evalExpr(FormCtx(formComponentId), staticTypeData, behaviour)
      case Else(field1, field2) =>
        val l = evalExpr(field1, staticTypeData, behaviour)
        val r = evalExpr(field2, staticTypeData, behaviour)
        l.orElse(r)
      case Value => dataBridge.valueValue
    }

  private def evalBooleanExpr(
    booleanExpr: BooleanExpr,
    staticTypeData: StaticTypeData,
    behaviour: Behaviour
  ): List[Boolean] =
    booleanExpr match {
      case Or(lhs, rhs) =>
        val l = evalBooleanExpr(lhs, staticTypeData, behaviour)
        val r = evalBooleanExpr(rhs, staticTypeData, behaviour)
        val zipped = (l, r) match {
          case ((padding :: Nil), _) if !lhs.containsRepeatingFieldRef(metadata) => l.zipAll(r, padding, false)
          case (_, (padding :: Nil)) if !rhs.containsRepeatingFieldRef(metadata) => l.zipAll(r, false, padding)
          case _                                                                 => l.zipAll(r, false, false)
        }
        zipped.map { case (l, r) => l || r }
      case And(lhs, rhs) =>
        val l = evalBooleanExpr(lhs, staticTypeData, behaviour)
        val r = evalBooleanExpr(rhs, staticTypeData, behaviour)
        val zipped = (l, r) match {
          case ((padding :: Nil), _) if !lhs.containsRepeatingFieldRef(metadata) => l.zipAll(r, padding, false)
          case (_, (padding :: Nil)) if !rhs.containsRepeatingFieldRef(metadata) => l.zipAll(r, false, padding)
          case _                                                                 => l.zipAll(r, false, false)
        }
        zipped.map { case (l, r) => l && r }
      case Equals(lhs, rhs) =>
        val l = evalExpr(lhs, staticTypeData, behaviour).applyStaticTypeData(staticTypeData)
        val r = evalExpr(rhs, staticTypeData, behaviour).applyStaticTypeData(staticTypeData)
        l.identical(r)
      case Not(be) => evalBooleanExpr(be, staticTypeData, behaviour).map(!_)
      case Contains(formCtx, expr) =>
        val formCtxValue = evalExpr(formCtx, staticTypeData, behaviour)
        val exprValue = evalExpr(expr).evaluationStatus
        formCtxValue.contains(exprValue, behaviour)
      case GreaterThan(lhs, rhs) =>
        val l = evalExpr(lhs, staticTypeData, behaviour).applyStaticTypeData(staticTypeData)
        val r = evalExpr(rhs, staticTypeData, behaviour).applyStaticTypeData(staticTypeData)
        l > r
      case GreaterThanOrEquals(lhs, rhs) =>
        val l = evalExpr(lhs, staticTypeData, behaviour).applyStaticTypeData(staticTypeData)
        val r = evalExpr(rhs, staticTypeData, behaviour).applyStaticTypeData(staticTypeData)
        l >= r
      case LessThan(lhs, rhs) =>
        val l = evalExpr(lhs, staticTypeData, behaviour).applyStaticTypeData(staticTypeData)
        val r = evalExpr(rhs, staticTypeData, behaviour).applyStaticTypeData(staticTypeData)
        l < r
      case LessThanOrEquals(lhs, rhs) =>
        val l = evalExpr(lhs, staticTypeData, behaviour).applyStaticTypeData(staticTypeData)
        val r = evalExpr(rhs, staticTypeData, behaviour).applyStaticTypeData(staticTypeData)
        l <= r
      case DateBefore(lhs, rhs) =>
        val l = evalDateExpr(lhs, staticTypeData, behaviour)
        val r = evalDateExpr(rhs, staticTypeData, behaviour)
        l before r
      case DateAfter(lhs, rhs) =>
        val l = evalDateExpr(lhs, staticTypeData, behaviour)
        val r = evalDateExpr(rhs, staticTypeData, behaviour)
        l after r
      case MatchRegex(expr, regex) => evalExpr(expr, staticTypeData, behaviour).matchRegex(regex)
      case IsTrue                  => true :: Nil
      case IsFalse                 => false :: Nil
      case DuplicateExists(formCtxs) =>
        evalDuplicateExists(formCtxs, staticTypeData) :: Nil
      case First(formCtx) =>
        val insideAtl = dataBridge.insideAtl(formCtx)
        isFirst(formCtx, staticTypeData, insideAtl, behaviour)
      case FormPhase(value) => evalFormPhase(value) :: Nil
      case HasAnswer(lhs, rhs) =>
        val evaluationStatusLeft: EvaluationStatus = evalExpr(lhs, staticTypeData, Behaviour.Default)
        val evaluationStatusRight = evalExpr(rhs, staticTypeData, Behaviour.LessThanCurrent)
        val hasAnswer = evaluationStatusRight.contains(evaluationStatusLeft, behaviour)
        val outsideAtl = dataBridge.outsideAtl(rhs)
        if (outsideAtl || behaviour == Behaviour.LessThanCurrent) {
          hasAnswer
        } else hasAnswer.exists(_ == true) :: Nil

      case In(expr, dataSource) =>
        evalIn(expr, dataSource, staticTypeData, behaviour)
      case IsLogin(loginInfo) => evalIsLogin(loginInfo) :: Nil
    }

  private def evalDateExpr(
    dateExpr: DateExpr,
    staticTypeData: StaticTypeData,
    behaviour: Behaviour
  ): EvaluationStatus =
    dateExpr match {
      case DateValueExpr(value) =>
        EvaluationStatus.DateResult.mkDate(value.toLocalDate(evaluationContext.formStartDate))
      case DateFormCtxVar(formCtx) => evalExpr(formCtx, behaviour)
      case DateExprWithOffset(dExpr, offset) =>
        evalDateExpr(dExpr, staticTypeData, behaviour)
          .mapDateResult(localDate => EvaluationStatus.DateResult.mkDate(DateExprEval.addOffset(localDate, offset)))

      case HmrcTaxPeriodCtx(FormCtx(formComponentId), hmrcTaxPeriodInfo) =>
        evalExpr(FormCtx(formComponentId), staticTypeData, behaviour) match {
          case EvaluationStatus.StringResult(period) =>
            val maybeObligationDetail: Option[ObligationDetail] =
              evaluationContext.thirdPartyData.obligations.findByFcPeriodKey(formComponentId, period)
            maybeObligationDetail
              .map { value =>
                EvaluationStatus.DateResult.mkDate(
                  hmrcTaxPeriodInfo match {
                    case HmrcTaxPeriodInfo.PeriodTo   => value.inboundCorrespondenceToDate
                    case HmrcTaxPeriodInfo.PeriodFrom => value.inboundCorrespondenceFromDate
                    case HmrcTaxPeriodInfo.PeriodDue  => value.inboundCorrespondenceDueDate
                  }
                )
              }
              .getOrElse(EvaluationStatus.Empty)
          case _ => EvaluationStatus.Empty
        }
      case DataRetrieveDateCtx(dataRetrieveId, attribute) =>
        val drId = dataBridge.liftDataRetrieveId(dataRetrieveId)
        DateExprEval.evalDataRetrieveDate(drId, attribute, evaluationContext)
      case DateIfElse(cond, field1, field2) =>
        evalBooleanExpr(cond, staticTypeData, behaviour) match {
          case cond :: Nil =>
            if (cond)
              evalDateExpr(field1, staticTypeData, behaviour)
            else
              evalDateExpr(field2, staticTypeData, behaviour)
          case conds =>
            EvaluationStatus.ListResult(conds.zipWithIndex.map { case (cond, index) =>
              val expr = if (cond) field1 else field2

              evalDateExpr(expr, staticTypeData, behaviour) match {
                case EvaluationStatus.ListResult(list) => list(index)
                case otherwise                         => otherwise
              }
            })
        }

      case DateOrElse(field1, field2) =>
        evalDateExpr(field1, staticTypeData, behaviour) match {
          case EvaluationStatus.Hidden | EvaluationStatus.Empty => evalDateExpr(field2, staticTypeData, behaviour)
          case otherwise                                        => otherwise
        }
      case DateConstructExpr(dateExpr, expr) =>
        val der: EvaluationStatus = evalDateExpr(dateExpr, staticTypeData, behaviour)
        val er: EvaluationStatus = evalExpr(expr, staticTypeData, behaviour)
        DateExprEval.evalDateConstructExpr(der, er)
      case EarliestOf(exprs) =>
        val evaluationStatuses = exprs.map(expr => evalDateExpr(expr, staticTypeData, behaviour))
        earliestOf(evaluationStatuses)
      case LatestOf(exprs) =>
        val evaluationStatuses = exprs.map(expr => evalDateExpr(expr, staticTypeData, behaviour))
        latestOf(evaluationStatuses)
    }
}
