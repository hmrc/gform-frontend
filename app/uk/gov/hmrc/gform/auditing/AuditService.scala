package uk.gov.hmrc.gform.auditing

import play.api.mvc.Request
import uk.gov.hmrc.gform.gformbackend.model.{EnvelopeId, Form, FormId}
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.DataEvent
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.http.HeaderCarrier

trait AuditService {

  def auditConnector : AuditConnector

  val formToMap : Form => Map[String, String] = {
    form =>
      val dataMap = Map(
        "FormId" -> form._id.value,
        "EnvelopeId" -> form.envelopeId.value,
        "FormTypeId" -> form.formData.formTypeId.value,
        "UserId" -> form.formData.userId.value, //TODO is userId required in the formData anymore.
        "CharacterSet" -> form.formData.characterSet
      )

      dataMap ++ form.formData.fields.map(x => x.id.value -> x.value).toMap
  }
  def sendSubmissionEvent(form: Form) = {
    sendEvent(formToMap(form))
  }

  private def sendEvent(detail: Map[String, String])(implicit hc : HeaderCarrier, authContext: AuthContext, request: Request[_]) =
    auditConnector.sendEvent(eventFor(detail))


  private def eventFor(detail: Map[String, String])(implicit hc : HeaderCarrier, authContext: AuthContext, request: Request[_]) = {
    DataEvent(
      auditSource = "GForm",
      auditType = "submission complete auditing",
      tags =   hc.headers.toMap,
      detail = detail ++ Map(
        "nino" -> authContext.principal.name.getOrElse(""),
        "vrn" -> authContext.principal.accounts.vat.map(_.vrn.vrn).getOrElse(""),
        "saUtr" -> authContext.principal.accounts.ated.getOrElse("").toString,
        "ctUtr" -> authContext.principal.accounts.ct.getOrElse("").toString,
        "deviceId" -> hc.deviceID.map(a => a).getOrElse("")
      )
    )
  }
}
