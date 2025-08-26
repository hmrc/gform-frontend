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
import uk.gov.hmrc.auth.core.retrieve.ItmpAddress
import uk.gov.hmrc.gform.addresslookup.PostcodeLookupRetrieve.AddressRecord
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.commons.HeaderCarrierUtil
import uk.gov.hmrc.gform.eval.ExpressionResult
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.gform.CustomerId
import uk.gov.hmrc.gform.models.ids.{ IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.mappings.{ IRCT, IRSA, NINO, VATReg }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.objectStore.File
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AuthInfo.ItmpDateOfBirth
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayInSummary.Yes
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthCtx, DataRetrieveCtx, Expr, FormComponent, FormComponentId, IncludeIf, IsAddress, IsMiniSummaryList, IsOverseasAddress, MiniSummaryList, MiniSummaryListValue, MiniSummaryRow }
import uk.gov.hmrc.gform.sharedmodel.{ AffinityGroupUtil, DataRetrieve, RetrieveDataType, SmartString, SubmissionRef, VariadicValue }
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
  )(implicit sse: SmartStringEvaluator): UserValues = {
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

    def getMslMap(mslComponents: List[(String, MiniSummaryList)]): Map[String, AuditMslRow] = {
      def evaluateSmartString(smartString: Option[SmartString], trim: Boolean): Option[String] =
        smartString match {
          case Some(ss) =>
            val value = sse.evalEnglish(ss, markDown = false)
            //TODO: Split by space and capitalize first letter of each item > index 0
            if (trim) Some(value.trim.replaceAll(" ", "")) else value
          case None => None
        }

      def getAuditMslRowFromExprResult(expressionResult: ExpressionResult, expr: Expr): AuditMslRow =
        expressionResult match {
          case ExpressionResult.NumberResult(value) => AuditMslRow(Some(value.toString()), None, None)
          case ExpressionResult.StringResult(value) =>
            expr match {
              case AuthCtx(authInfo) =>
                if (authInfo == ItmpDateOfBirth) {
                  val itmpDateOfBirth = thirdPartyData.itmpRetrievals match {
                    case Some(retrievals) =>
                      retrievals.itmpDateOfBirth match {
                        case Some(localDate) =>
                          Some(localDate)
                        case None => None
                      }
                    case None => None
                  }
                  itmpDateOfBirth match {
                    case Some(dateOfBirth) => AuditMslRow(None, None, Some(dateOfBirth))
                    case None              => AuditMslRow(None, None, None)
                  }
                } else {
                  AuditMslRow(Some(value), None, None)
                }
              case _ =>
                AuditMslRow(Some(value), None, None)
            }
          case ExpressionResult.DateResult(localDate) =>
            AuditMslRow(
              None,
              None,
              Some(localDate)
            )
          case ExpressionResult.TaxPeriodResult(month, year) =>
            AuditMslRow(Some(s"${year.toString}-${month.toString}"), None, None)
          case p @ ExpressionResult.PeriodResult(_) => AuditMslRow(Some(p.asString), None, None)
          case ExpressionResult.OptionResult(value) => AuditMslRow(Some(value.mkString(",")), None, None)
          case ExpressionResult.ListResult(_)       => AuditMslRow(Some("List result"), None, None) //TODO: handle this
          case ExpressionResult.AddressResult(addressRes) =>
            expr match {
              case AuthCtx(_) =>
                val itmpAddress: Option[ItmpAddress] = thirdPartyData.itmpRetrievals match {
                  case Some(retrievals) =>
                    retrievals.itmpAddress match {
                      case Some(address) => Some(address)
                      case None          => None
                    }
                  case None => None
                }
                itmpAddress match {
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
                        AuditMslRow(None, Some(auditAddress), None)
                      case _ => AuditMslRow(None, None, None)
                    }
                  case None =>
                    AuditMslRow(None, None, None)
                }
              case DataRetrieveCtx(id, _) =>
                thirdPartyData.dataRetrieve match {
                  case Some(dr) =>
                    dr.get(id) match {
                      case Some(drr) =>
                        drr.data match {
                          case RetrieveDataType.ObjectType(data) =>
                            val line1 = data.get(DataRetrieve.Attribute("address_line_1"))
                            val postcode = data.get(DataRetrieve.Attribute("postal_code"))
                            (line1, postcode) match {
                              case (Some(line1), Some(postcode)) =>
                                val auditAddress = AuditAddress(
                                  line1,
                                  data.get(DataRetrieve.Attribute("address_line_2")),
                                  data.get(DataRetrieve.Attribute("po_box")),
                                  data.get(DataRetrieve.Attribute("locality")),
                                  postcode,
                                  data.get(DataRetrieve.Attribute("region")),
                                  data.get(DataRetrieve.Attribute("country")),
                                  None
                                )
                                AuditMslRow(None, Some(auditAddress), None)
                              case _ => AuditMslRow(None, None, None)
                            }
                          case RetrieveDataType.ListType(_) => AuditMslRow(None, None, None) //TODO: Handle this
                        }
                      case None => AuditMslRow(None, None, None)
                    }
                  case None => AuditMslRow(None, None, None)
                }
              case _ =>
                AuditMslRow(
                  None,
                  Some(AuditAddress(addressRes.head, None, None, None, "BS22FR", "Bristol", "United Kingdom", None)),
                  None
                )
            }
          case _ => AuditMslRow(None, None, None)
        }

      def getAuditMslRow(mslValue: MiniSummaryListValue): AuditMslRow =
        mslValue match {
          case MiniSummaryListValue.AnyExpr(expr) =>
            getAuditMslRowFromExprResult(
              formModelVisibilityOptics.evalAndApplyTypeInfoFirst(expr).expressionResult,
              expr
            )
          case MiniSummaryListValue.Reference(expr) =>
            getAuditMslRowFromExprResult(
              formModelVisibilityOptics.evalAndApplyTypeInfoFirst(expr).expressionResult,
              expr
            )
        }

      def shouldInclude(includeIf: Option[IncludeIf]): Boolean =
        includeIf.fold(true)(formModelVisibilityOptics.evalIncludeIfExpr(_, None))

      mslComponents.flatMap { msl =>
        msl._2.rows
          .collect {
            //TODO: Where there is no key, we can't just use msl._1 in case of duplication. We should pass in an index somehow or use a UUID
            case MiniSummaryRow.ValueRow(key, value, includeIf, _, _) if shouldInclude(includeIf) =>
              val auditKey = evaluateSmartString(key, trim = true).fold(msl._1)(k => s"${msl._1}$k")
              val auditMslRow = getAuditMslRow(value)
              auditKey -> auditMslRow
            case MiniSummaryRow.SmartStringRow(key, value, includeIf, _, _) if shouldInclude(includeIf) =>
              val auditKey = evaluateSmartString(key, trim = true).fold(msl._1)(k => s"${msl._1}$k")
              auditKey -> AuditMslRow(evaluateSmartString(Some(value), trim = false), None, None)
          }
          .filter(kv => kv._2 != AuditMslRow(None, None, None))
      }.toMap
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

    //get info components
//    val infoComponents: List[FormComponent] =
//      formModelVisibilityOptics.allFormComponents.filter {
//        case IsInformationMessage(_) => true
//        case _ => false
//      }

    UserValues(
      getValueMap(componentsExclAddresses),
      getAddressMap(addressComponents),
      getMslMap(mslComponents)
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
  )(implicit ec: ExecutionContext, hc: HeaderCarrier, sse: SmartStringEvaluator): Unit =
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
  )(implicit hc: HeaderCarrier, sse: SmartStringEvaluator): ExtendedDataEvent =
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

    val summaryItemsOnlyJsObj: JsObject =
      if (detail.mslRows.nonEmpty)
        Json.obj(
          "SummaryItems" -> Json.obj(
            detail.mslRows.map { case (key, auditMslRow) =>
              val value: Json.JsValueWrapper = auditMslRow match {
                case AuditMslRow(Some(text), None, None)    => text
                case AuditMslRow(None, Some(address), None) => Json.toJson(address)
                case AuditMslRow(None, None, Some(date)) =>
                  s"${date.getYear.toString}-${date.getMonthValue.toString}-${date.getDayOfMonth.toString}"
                case AuditMslRow(Some(text), _, _)       => text
                case AuditMslRow(None, Some(address), _) => Json.toJson(address)
                case _                                   => ""
              }
              key -> value
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
    ) ++ userAddressesJsObj ++ summaryItemsOnlyJsObj ++
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
  mslRows: Map[String, AuditMslRow]
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

case class AuditMslRow(
  text: Option[String],
  address: Option[AuditAddress],
  date: Option[LocalDate]
)

object AuditMslRow {
  implicit val format: Format[AuditMslRow] = Json.format[AuditMslRow]
}
