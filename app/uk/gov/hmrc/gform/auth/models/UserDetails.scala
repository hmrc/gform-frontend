package uk.gov.hmrc.gform.auth.models

import play.api.libs.json.{JsError, JsSuccess, Reads}
import uk.gov.hmrc.gform.models.UserId
import uk.gov.hmrc.gform.models.userdetails.AffinityGroup

case class UserDetails(userId: UserId, affinityGroup: AffinityGroup)

object UserDetails {
  implicit val reads: Reads[UserDetails] = Reads[UserDetails] { x =>
    x.asOpt[UserId] match {
      case Some(userId) =>
        x.asOpt[AffinityGroup] match {
          case Some(affinGroup) => JsSuccess(UserDetails(userId, affinGroup))
          case None => JsError("No AffinityGroup is present")
        }
      case None => JsError("No UserId is present")
    }
  }
}
