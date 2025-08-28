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

package uk.gov.hmrc.gform.auditing

import cats.data.NonEmptyList
import play.api.libs.json.{ Format, JsNumber, JsObject, Json }
import uk.gov.hmrc.gform.addresslookup.PostcodeLookupRetrieve.AddressRecord
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.commons.HeaderCarrierUtil
import uk.gov.hmrc.gform.eval.ExpressionResult
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.gform.CustomerId
import uk.gov.hmrc.gform.models.ids.{ IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.mappings.{ IRCT, IRSA, NINO, VATReg }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.objectStore.File
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AuthInfo.ItmpDateOfBirth
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayInSummary.Yes
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthCtx, DataRetrieveCtx, Expr, FormComponent, FormComponentId, IncludeIf, InformationMessage, IsAddress, IsInformationMessage, IsMiniSummaryList, IsOverseasAddress, MiniSummaryList, MiniSummaryListValue, MiniSummaryRow }
import uk.gov.hmrc.gform.sharedmodel.{ AffinityGroupUtil, DataRetrieve, DataRetrieveId, LangADT, RetrieveDataType, SmartString, SubmissionRef, VariadicValue }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.{ DataEvent, ExtendedDataEvent }

import java.time.LocalDate
import scala.concurrent.ExecutionContext

trait AuditService {

  def auditConnector: AuditConnector

  def getUserValues[D <: DataOrigin](
    thirdPartyData: ThirdPartyData,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit sse: SmartStringEvaluator, l: LangADT): UserValues = {
    import scala.language.implicitConversions
    implicit def toOpt(str: String): Option[String] = Option.unless(str.isBlank)(str)

    def getValueMap(components: List[FormComponent]): Map[String, String] = components
      .flatMap(_.multiValueId.toModelComponentIds)
      .flatMap { modelComponentId =>
        formModelVisibilityOptics.data.get(modelComponentId).map(modelComponentId -> _)
      }
      .map { case (modelComponentId, variadicValue) =>
        modelComponentId.toMongoIdentifier -> variadicValue.toSeq.mkString(",")
      }
      .toMap

    def getAddressMap(addressComponents: List[FormComponent]) = {
      def getThirdPartyDataAddresses: Map[String, AuditAddress] = {
        def fromEntered(maybeFormData: Option[FormData], maybeUprn: Option[String]): AuditAddress = {
          def findByAtom(atom: String): String =
            maybeFormData
              .flatMap { formData =>
                formData.toData.find { case (mcId, _) => mcId.isAtomic(atom) }
              }
              .map { case (_, value) => value }
              .getOrElse("")

          val country: String = AuditAddress.determineCountry(findByAtom("uk"), findByAtom("country"))

          AuditAddress(
            findByAtom("street1"),
            findByAtom("street2"),
            findByAtom("street3"),
            findByAtom("street4"),
            findByAtom("postcode"),
            findByAtom("town"),
            country,
            maybeUprn
          )
        }

        def fromLookup(lookupAddress: AddressRecord): AuditAddress =
          AuditAddress(
            lookupAddress.address.line1,
            lookupAddress.address.line2,
            lookupAddress.address.line3,
            lookupAddress.address.line4,
            lookupAddress.address.postcode,
            lookupAddress.address.town,
            lookupAddress.address.country.name,
            lookupAddress.uprn.map(_.toString)
          )

        def findUprn(fcId: FormComponentId, addressId: String): Option[String] = {
          val maybeAddresses: Option[NonEmptyList[AddressRecord]] =
            thirdPartyData.postcodeLookup.flatMap(_.get(fcId)).map(_.response).flatMap(_.addresses)
          maybeAddresses.flatMap(_.find(addr => addr.id == addressId).flatMap(found => found.uprn.map(_.toString)))
        }

        val confirmedAddresses: Set[FormComponentId] = thirdPartyData.confirmedAddresses.getOrElse(Set.empty)
        val selectedAddresses: Map[FormComponentId, String] = thirdPartyData.selectedAddresses.getOrElse(Map.empty)
        val enteredAddresses: Map[FormComponentId, FormData] = thirdPartyData.enteredAddresses.getOrElse(Map.empty)

        confirmedAddresses.map { fcId =>
          if (enteredAddresses.contains(fcId)) {
            val maybeUprn: Option[String] = selectedAddresses.get(fcId).flatMap(addressId => findUprn(fcId, addressId))

            fcId.modelComponentId.toMongoIdentifier -> fromEntered(enteredAddresses.get(fcId), maybeUprn)
          } else {
            val maybeAddressList: Option[NonEmptyList[AddressRecord]] =
              thirdPartyData.postcodeLookup
                .flatMap(_.get(fcId))
                .map(_.response)
                .flatMap(_.addresses)

            val maybeAddress: Option[AddressRecord] =
              maybeAddressList.flatMap(_.find(_.id == selectedAddresses.getOrElse(fcId, "")))

            fcId.modelComponentId.toMongoIdentifier -> maybeAddress.map(fromLookup).getOrElse(AuditAddress.empty)
          }
        }.toMap
      }

      def toAuditAddress(values: List[(ModelComponentId, VariadicValue)]): AuditAddress = {
        def findByAtom(primaryAtom: String, maybeAlternative: Option[String] = None): String = {
          val maybePrimary: Option[(ModelComponentId, VariadicValue)] = values.find { case (mcId, _) =>
            mcId.isAtomic(primaryAtom)
          }
          val result: Option[(ModelComponentId, VariadicValue)] = (maybePrimary, maybeAlternative) match {
            case (None, Some(alt)) => values.find { case (mcId, _) => mcId.isAtomic(alt) }
            case (_, _)            => maybePrimary
          }
          result.map { case (_, value) => value.toSeq.mkString(" ") }.getOrElse("")
        }

        val country: String = AuditAddress.determineCountry(findByAtom("uk"), findByAtom("country"))

        AuditAddress(
          findByAtom("street1", Some("line1")),
          findByAtom("street2", Some("line2")),
          findByAtom("street3", Some("line3")),
          findByAtom("street4"),
          findByAtom("postcode"),
          findByAtom("city"),
          country,
          None
        )
      }

      val addressOnlyValues: List[(ModelComponentId, VariadicValue)] = addressComponents
        .flatMap(_.multiValueId.toModelComponentIds)
        .flatMap { modelComponentId =>
          formModelVisibilityOptics.data.get(modelComponentId).map(modelComponentId -> _)
        }

      val groupedAddressValues: Map[IndexedComponentId, List[(ModelComponentId, VariadicValue)]] =
        addressOnlyValues.groupBy(_._1.indexedComponentId)

      val addressesFromComponents: Map[String, AuditAddress] = groupedAddressValues.map { case (_, variadicValues) =>
        variadicValues.head._1.toMongoIdentifier -> toAuditAddress(variadicValues)
      }

      addressesFromComponents ++ getThirdPartyDataAddresses
    }

    def evaluateSmartString(smartString: Option[SmartString], trim: Boolean): Option[String] =
      smartString.map { ss =>
        val value = ss.englishValue()
        if (trim) {
          value.trim
            .split("\\s+")
            .filter(!_.isEmpty)
            .map(_.capitalize)
            .mkString("")
        } else {
          value
        }
      }

    def getSummaryItemsMap(
      mslComponents: List[(String, MiniSummaryList)],
      infoComponents: List[(String, InformationMessage)]
    ): Map[String, SummaryAuditItem] = {

      def processExpressionResult(expressionResult: ExpressionResult, expr: Expr): SummaryAuditItem = {

        def extractItmpData[T](extractor: ItmpRetrievals => Option[T]): Option[T] =
          thirdPartyData.itmpRetrievals.flatMap(extractor)

        def extractDataRetrieveAddress(id: DataRetrieveId): List[AuditAddress] =
          thirdPartyData.dataRetrieve
            .flatMap(_.get(id))
            .map(_.data)
            .toList
            .flatMap {
              case RetrieveDataType.ObjectType(data) =>
                createAuditAddressFromData(data).toList
              case RetrieveDataType.ListType(dataList) =>
                dataList.flatMap(createAuditAddressFromData)
            }

        def createAuditAddressFromData(data: Map[DataRetrieve.Attribute, String]): Option[AuditAddress] = {
          val line1 = data.get(DataRetrieve.Attribute("address_line_1"))
          val postcode = data.get(DataRetrieve.Attribute("postal_code"))

          (line1, postcode) match {
            case (Some(line1Value), Some(postcodeValue)) =>
              Some(
                AuditAddress(
                  line1Value,
                  data.get(DataRetrieve.Attribute("address_line_2")),
                  data.get(DataRetrieve.Attribute("po_box")),
                  data.get(DataRetrieve.Attribute("locality")),
                  postcodeValue,
                  data.get(DataRetrieve.Attribute("region")),
                  data.get(DataRetrieve.Attribute("country")),
                  None
                )
              )
            case _ => None
          }
        }

        expressionResult match {
          case ExpressionResult.NumberResult(value) =>
            SummaryAuditItem.fromText(value.toString())

          case ExpressionResult.StringResult(value) =>
            expr match {
              case AuthCtx(authInfo) if authInfo == ItmpDateOfBirth =>
                extractItmpData(_.itmpDateOfBirth) match {
                  case Some(dateOfBirth) => SummaryAuditItem.fromDate(dateOfBirth)
                  case None              => SummaryAuditItem.empty
                }
              case _ => SummaryAuditItem.fromText(value)
            }

          case ExpressionResult.DateResult(localDate) =>
            SummaryAuditItem.fromDate(localDate)

          case ExpressionResult.TaxPeriodResult(month, year) =>
            SummaryAuditItem.fromText(s"$year-$month")

          case p @ ExpressionResult.PeriodResult(_) =>
            SummaryAuditItem.fromText(p.asString)

          case ExpressionResult.OptionResult(values) =>
            SummaryAuditItem.fromTextList(values.toList)

          case ExpressionResult.ListResult(results) =>
            val processedResults = results.map(processExpressionResult(_, expr))
            SummaryAuditItem.combineAll(processedResults)
          case ExpressionResult.AddressResult(_) =>
            expr match {
              case AuthCtx(_) =>
                extractItmpData(_.itmpAddress) match {
                  case Some(address) =>
                    (address.line1, address.postCode) match {
                      case (Some(line1), Some(postcode)) =>
                        val auditAddress = AuditAddress(
                          line1,
                          address.line2,
                          address.line3,
                          address.line4,
                          postcode,
                          address.line5,
                          address.countryName,
                          None
                        )
                        SummaryAuditItem.fromAddress(auditAddress)
                      case _ => SummaryAuditItem.empty
                    }
                  case None => SummaryAuditItem.empty
                }

              case DataRetrieveCtx(id, _) =>
                val addresses = extractDataRetrieveAddress(id)
                SummaryAuditItem.fromAddressList(addresses)

              case _ => SummaryAuditItem.empty
            }

          case _ => SummaryAuditItem.empty
        }
      }

      def processMinSummaryListValue(mslValue: MiniSummaryListValue): SummaryAuditItem =
        mslValue match {
          case MiniSummaryListValue.AnyExpr(expr) =>
            val result = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(expr)
            processExpressionResult(result.expressionResult, expr)
          case MiniSummaryListValue.Reference(expr) =>
            val result = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(expr)
            processExpressionResult(result.expressionResult, expr)
        }

      def shouldIncludeRow(includeIf: Option[IncludeIf]): Boolean =
        includeIf.fold(true)(formModelVisibilityOptics.evalIncludeIfExpr(_, None))

      val mslSummaryItems = for {
        (componentId, miniSummaryList) <- mslComponents
        row                            <- miniSummaryList.rows
      } yield row match {
        case MiniSummaryRow.ValueRow(key, value, includeIf, _, _) if shouldIncludeRow(includeIf) =>
          val auditKey = evaluateSmartString(key, trim = true)
            .fold(componentId)(k => s"$componentId$k")
          val summaryItem = processMinSummaryListValue(value)
          Some(auditKey -> summaryItem)

        case MiniSummaryRow.SmartStringRow(key, value, includeIf, _, _) if shouldIncludeRow(includeIf) =>
          val auditKey = evaluateSmartString(key, trim = true)
            .fold(componentId)(k => s"$componentId$k")
          val textValue = evaluateSmartString(Some(value), trim = false)
            .map(SummaryAuditItem.fromText)
            .getOrElse(SummaryAuditItem.empty)
          Some(auditKey -> textValue)
        case _ => None
      }

      val infoSummaryItems = for {
        (componentId, infoComponent) <- infoComponents
      } yield infoComponent.summaryValue match {
        case Some(summaryValue) =>
          if (
            summaryValue.rawValue(
              formModelVisibilityOptics.booleanExprResolver.resolve(_)
            ) == "{0}" && summaryValue.allInterpolations.headOption.nonEmpty
          ) {
            val result = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(summaryValue.allInterpolations.head)
            val summaryItem = processExpressionResult(result.expressionResult, summaryValue.allInterpolations.head)
            Some(componentId -> summaryItem)
          } else {
            val textValue = evaluateSmartString(
              Some(infoComponent.summaryValue.fold(infoComponent.infoText)(sv => sv)),
              trim = false
            ).map(SummaryAuditItem.fromText).getOrElse(SummaryAuditItem.empty)
            Some(componentId -> textValue)
          }
        case None => None
      }

      (mslSummaryItems ++ infoSummaryItems).flatten.filter { case (_, summaryItem) => !summaryItem.isEmpty }.toMap
    }

    val addressComponents: List[FormComponent] = formModelVisibilityOptics.allFormComponents.filter {
      case IsAddress(_) | IsOverseasAddress(_) => true
      case _                                   => false
    }

    val componentsExclAddresses: List[FormComponent] =
      formModelVisibilityOptics.allFormComponents.diff(addressComponents)

    val mslComponents: List[(String, MiniSummaryList)] =
      formModelVisibilityOptics.allFormComponents
        .collect { case fc @ IsMiniSummaryList(msl) =>
          fc.id.value -> msl
        }
        .filter(t => t._2.displayInSummary == Yes)

    val infoComponents: List[(String, InformationMessage)] =
      formModelVisibilityOptics.allFormComponents
        .collect { case fc @ IsInformationMessage(info) =>
          fc.id.value -> info
        }
        .filter(t => t._2.summaryValue.nonEmpty)

    UserValues(
      getValueMap(componentsExclAddresses),
      getAddressMap(addressComponents),
      getSummaryItemsMap(mslComponents, infoComponents)
    )
  }

  def sendFormCreateEvent[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formCreated", form, UserValues.empty, retrievals, CustomerId.empty, List.empty)

  def sendFormResumeEvent[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formResumed", form, UserValues.empty, retrievals, CustomerId.empty, List.empty)

  def sendSubmissionEvent[D <: DataOrigin](
    form: Form,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId,
    envelopeFiles: List[File]
  )(implicit ec: ExecutionContext, hc: HeaderCarrier, sse: SmartStringEvaluator, l: LangADT): Unit =
    sendEvent(
      "formSubmitted",
      form,
      getUserValues(form.thirdPartyData, formModelVisibilityOptics),
      retrievals,
      customerId,
      envelopeFiles
    )

  def formSavedEvent[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formSaved", form, UserValues.empty, retrievals, CustomerId.empty, List.empty)

  def sendFormTimoutEvent[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formTimeout", form, UserValues.empty, retrievals, CustomerId.empty, List.empty)

  def sendFormSignOut[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formSignOut", form, UserValues.empty, retrievals, CustomerId.empty, List.empty)

  private def sendEvent(
    auditType: String,
    form: Form,
    detail: UserValues,
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId,
    envelopeFiles: List[File]
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    auditConnector.sendExplicitAudit(auditType, details(form, detail, retrievals, customerId, envelopeFiles))

  def calculateSubmissionEvent[D <: DataOrigin](
    form: Form,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId
  )(implicit hc: HeaderCarrier, sse: SmartStringEvaluator, l: LangADT): ExtendedDataEvent =
    eventFor(form, getUserValues(form.thirdPartyData, formModelVisibilityOptics), retrievals, customerId)

  private def eventFor(
    form: Form,
    detail: UserValues,
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId
  )(implicit hc: HeaderCarrier) =
    ExtendedDataEvent(
      auditSource = "Gform-Frontend",
      auditType = "formSubmitted",
      detail = details(form, detail, retrievals, customerId, List.empty)
    )

  private def details(
    form: Form,
    detail: UserValues,
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId,
    envelopeFiles: List[File]
  )(implicit hc: HeaderCarrier) = {

    val userInfo =
      retrievals match {
        case mr: AuthenticatedRetrievals =>
          Json.toJson(
            Map(
              "email"          -> mr.getEmail.toString,
              "ggLogin"        -> "true",
              "nino"           -> mr.getTaxIdValue(NINO()),
              "vrn"            -> mr.getTaxIdValue(VATReg()),
              "saUtr"          -> mr.getTaxIdValue(IRSA()),
              "ctUtr"          -> mr.getTaxIdValue(IRCT()),
              "deviceId"       -> hc.deviceID.getOrElse(""),
              "affinityGroup"  -> AffinityGroupUtil.affinityGroupName(mr.affinityGroup),
              "credentialRole" -> mr.getCredentialRole.map(_.toString).getOrElse("")
            ).filter(values => values._2.nonEmpty)
          )
        case r: VerifyRetrievals =>
          Json.toJson(
            Map(
              "nino"     -> r.getTaxIdValue(NINO()),
              "deviceId" -> hc.deviceID.getOrElse("")
            ).filter(values => values._2.nonEmpty)
          )
        case AnonymousRetrievals(_) =>
          Json.toJson(
            Map(
              "deviceId" -> hc.deviceID.getOrElse("")
            ).filter(values => values._2.nonEmpty)
          )
        case er: EmailRetrievals =>
          Json.toJson(
            Map(
              "email"      -> er.getEmail.toString,
              "emailLogin" -> "true",
              "deviceId"   -> hc.deviceID.getOrElse("")
            ).filter(values => values._2.nonEmpty)
          )
      }

    val userValues = Json.toJson(detail.userValues.filter(values => values._2.nonEmpty))
    val envelopeFilesJsObj =
      if (envelopeFiles.nonEmpty)
        Json.obj(
          "FileInfo" -> Json.obj(
            "numberOfFiles" -> JsNumber(envelopeFiles.size),
            "files"         -> envelopeFiles.map(file => Json.obj("name" -> file.fileName, "length" -> JsNumber(file.length)))
          )
        )
      else
        Json.obj()

    val userAddressesJsObj: JsObject =
      if (detail.userAddresses.nonEmpty)
        Json.obj(
          "UserAddresses" -> detail.userAddresses
        )
      else
        Json.obj()

    val summaryItemsJsObj: JsObject =
      if (detail.summaryItems.nonEmpty)
        Json.obj(
          "SummaryItems" -> Json.obj(
            detail.summaryItems.collect {
              case (key, summaryItem) if !summaryItem.isEmpty =>
                val jsValue: Json.JsValueWrapper = (summaryItem.text, summaryItem.address, summaryItem.date) match {
                  case (Some(texts), None, None) =>
                    if (texts.size == 1) texts.head else Json.toJson(texts)
                  case (None, Some(addresses), None) =>
                    if (addresses.size == 1) Json.toJson(addresses.head) else Json.toJson(addresses)
                  case (None, None, Some(dates)) =>
                    if (dates.size == 1) {
                      dates.head.format(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE)
                    } else {
                      Json.toJson(dates.map(_.format(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE)))
                    }
                  case (Some(texts), _, _) =>
                    if (texts.size == 1) texts.head else Json.toJson(texts)
                  case (None, Some(addresses), _) =>
                    if (addresses.size == 1) Json.toJson(addresses.head) else Json.toJson(addresses)
                  case _ => ""
                }
                key -> jsValue
            }.toSeq: _*
          )
        )
      else
        Json.obj()

    Json.obj(
      "FormId"         -> form._id.value,
      "EnvelopeId"     -> form.envelopeId.value,
      "FormTemplateId" -> form.formTemplateId.value,
      "UserId"         -> form.userId.value,
      "CustomerId"     -> customerId.id,
      "UserValues"     -> userValues
    ) ++ userAddressesJsObj ++ summaryItemsJsObj ++
      Json.obj(
        "UserInfo"      -> userInfo,
        "SubmissionRef" -> SubmissionRef(form.envelopeId).value
      ) ++ envelopeFilesJsObj
  }

  def sendSubmissionEventHashed(hashedValue: String, formAsString: String, eventId: String)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ) =
    sendHashedValues(hashedValue, formAsString, eventId)

  private def sendHashedValues(hash: String, formAsString: String, eventId: String)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ) = auditConnector.sendEvent(hashedValueEvent(hash, formAsString, eventId))

  private def hashedValueEvent(hashedValue: String, formString: String, eventId: String)(implicit hc: HeaderCarrier) =
    DataEvent(
      auditSource = "GForm",
      auditType = "printedReturnNonrepudiation",
      tags = HeaderCarrierUtil.allHeadersFromHC().toMap,
      eventId = eventId,
      detail = Map(
        "hashType"    -> "sha256",
        "hashedValue" -> hashedValue,
        "formData"    -> formString
      )
    )
}

case class UserValues(
  userValues: Map[String, String],
  userAddresses: Map[String, AuditAddress],
  summaryItems: Map[String, SummaryAuditItem]
)

object UserValues {
  lazy final val empty: UserValues = UserValues(Map.empty, Map.empty, Map.empty)
}

case class AuditAddress(
  addressLine1: String,
  addressLine2: Option[String],
  addressLine3: Option[String],
  addressLine4: Option[String],
  postCode: String,
  country: Option[String],
  uprn: Option[String]
)

object AuditAddress {
  implicit val format: Format[AuditAddress] = Json.format[AuditAddress]
  lazy final val empty: AuditAddress = AuditAddress("", None, None, None, "", None, None)
  private lazy final val UNITED_KINGDOM: String = "United Kingdom"

  def determineCountry(isUk: String, country: String): String = if (isUk == "true") UNITED_KINGDOM else country

  def apply(
    al1: String,
    al2: Option[String],
    al3: Option[String],
    al4: Option[String],
    postCode: String,
    town: Option[String],
    country: Option[String],
    uprn: Option[String]
  ): AuditAddress =
    (al2, al3, al4, town) match {
      case (_, _, Some(l4), Some(t))      => AuditAddress(al1, al2, al3, Some(s"$l4 $t"), postCode, country, uprn)
      case (_, Some(_), None, Some(t))    => AuditAddress(al1, al2, al3, Some(t), postCode, country, uprn)
      case (Some(_), None, None, Some(t)) => AuditAddress(al1, al2, Some(t), None, postCode, country, uprn)
      case (None, None, None, Some(t))    => AuditAddress(al1, Some(t), None, None, postCode, country, uprn)
      case (_, _, _, None)                => AuditAddress(al1, al2, al3, al4, postCode, country, uprn)
    }
}

case class SummaryAuditItem(
  text: Option[List[String]],
  address: Option[List[AuditAddress]],
  date: Option[List[LocalDate]]
) {
  def isEmpty: Boolean = text.isEmpty && address.isEmpty && date.isEmpty

  def combine(other: SummaryAuditItem): SummaryAuditItem = SummaryAuditItem(
    text = (text, other.text) match {
      case (Some(t1), Some(t2)) => Some((t1 ++ t2).distinct)
      case (Some(t1), None)     => Some(t1)
      case (None, Some(t2))     => Some(t2)
      case (None, None)         => None
    },
    address = (address, other.address) match {
      case (Some(a1), Some(a2)) => Some((a1 ++ a2).distinct)
      case (Some(a1), None)     => Some(a1)
      case (None, Some(a2))     => Some(a2)
      case (None, None)         => None
    },
    date = (date, other.date) match {
      case (Some(d1), Some(d2)) => Some((d1 ++ d2).distinct)
      case (Some(d1), None)     => Some(d1)
      case (None, Some(d2))     => Some(d2)
      case (None, None)         => None
    }
  )
}

object SummaryAuditItem {
  implicit val format: Format[SummaryAuditItem] = Json.format[SummaryAuditItem]

  val empty: SummaryAuditItem = SummaryAuditItem(None, None, None)

  def fromText(text: String): SummaryAuditItem = SummaryAuditItem(Some(List(text)), None, None)

  def fromAddress(address: AuditAddress): SummaryAuditItem = SummaryAuditItem(None, Some(List(address)), None)

  def fromDate(date: LocalDate): SummaryAuditItem = SummaryAuditItem(None, None, Some(List(date)))

  def fromTextList(texts: List[String]): SummaryAuditItem =
    if (texts.nonEmpty) SummaryAuditItem(Some(texts.distinct), None, None) else empty

  def fromAddressList(addresses: List[AuditAddress]): SummaryAuditItem =
    if (addresses.nonEmpty) SummaryAuditItem(None, Some(addresses.distinct), None) else empty

  def fromDateList(dates: List[LocalDate]): SummaryAuditItem =
    if (dates.nonEmpty) SummaryAuditItem(None, None, Some(dates.distinct)) else empty

  def combineAll(items: List[SummaryAuditItem]): SummaryAuditItem = {
    val nonEmptyItems = items.filter(!_.isEmpty)
    if (nonEmptyItems.isEmpty) {
      empty
    } else {
      nonEmptyItems.reduce(_.combine(_))
    }
  }
}
