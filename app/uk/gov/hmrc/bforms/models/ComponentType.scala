package uk.gov.hmrc.bforms.models

import julienrf.json.derived
import play.api.libs.json.OFormat

/**
  * Created by dimitra on 21/03/17.
  */
sealed trait ComponentType

case object Text extends ComponentType

case object Date extends ComponentType

case object Address extends ComponentType

object ComponentType {
  implicit val format: OFormat[ComponentType] = derived.oformat
}