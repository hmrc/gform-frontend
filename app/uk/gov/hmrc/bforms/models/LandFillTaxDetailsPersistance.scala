package uk.gov.hmrc.bforms.models

/**
  * Created by daniel-connelly on 05/01/17.
  */
case class LandFillTaxDetailsPersistance(firstName : FirstName, secondName : SecondName){

}

class FirstName(val value:String) extends AnyVal
class SecondName(val value:String) extends AnyVal


