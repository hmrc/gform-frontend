/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models.mappings

sealed trait ServiceNameAndTaxId

sealed trait ServiceNameAndId extends ServiceNameAndTaxId
case class IRSA(name: String = "IR-SA", id: String = "UTR") extends ServiceNameAndId
case class IRCT(name: String = "IR-CT", id: String = "UTR") extends ServiceNameAndId
case class HMRCOBTDSORG(name: String = "HMRC-OBTDS-ORG", id: String = "EtmpRegistrationNumber") extends ServiceNameAndId

sealed trait TaxIdentifier extends ServiceNameAndTaxId
case class NINO(id: String = "NINO") extends TaxIdentifier
case class VATReg(id: String = "VATRegNo") extends TaxIdentifier
