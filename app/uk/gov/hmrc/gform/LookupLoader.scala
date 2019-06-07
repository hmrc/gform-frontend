/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform

import com.miguelfonseca.completely.AutocompleteEngine
import com.miguelfonseca.completely.text.analyze.tokenize.WordTokenizer
import com.miguelfonseca.completely.text.analyze.transform.LowerCaseTransformer
import kantan.csv._
import kantan.csv.ops._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Register
import uk.gov.hmrc.gform.lookup.{ AjaxLookup, LookupAdapter, LookupId, LookupLabel, LookupRecord, LookupType, RadioLookup, ShowAll }

class LookupLoader {

  implicit private val lookupIdCellDecoder: CellDecoder[LookupId] = implicitly[CellDecoder[String]].map(LookupId)
  implicit private val lookupLabelCellDecoder: CellDecoder[LookupLabel] =
    implicitly[CellDecoder[String]].map(LookupLabel)

  private def read(
    filename: String,
    id: String,
    value: String,
    mkLookupType: Map[LookupLabel, LookupId] => LookupType): LookupType = {

    val headerDecoder = HeaderDecoder.decoder(value, id)((_: LookupLabel) -> (_: LookupId))
    val lookup = getClass.getClassLoader.getResourceAsStream("lookup/" + filename)
    mkLookupType(
      lookup
        .unsafeReadCsv[List, (LookupLabel, LookupId)](rfc.withHeader)(headerDecoder, implicitly, implicitly)
        .toMap
    )
  }

  private def mkAjaxLookup(showAll: ShowAll)(m: Map[LookupLabel, LookupId]): AjaxLookup =
    AjaxLookup(m, mkAutocoplete(m), showAll)
  private def mkRadioLookup(m: Map[LookupLabel, LookupId]): RadioLookup = RadioLookup(m)

  // format: off
  private val cashType                 = read("BCD-CashType.csv",                 "ID",           "Name", mkRadioLookup)
  private val country                  = read("BCD-Country.csv",                  "CountryCode",  "Name", mkAjaxLookup(ShowAll.Disabled))
  private val currency                 = read("BCD-Currency.csv",                 "CurrencyCode", "Name", mkAjaxLookup(ShowAll.Disabled))
  private val intent                   = read("BCD-Intent.csv",                   "ID",           "Name", mkRadioLookup)
  private val intercept                = read("BCD-Intercept.csv",                "ID",           "Name", mkRadioLookup)
  private val origin                   = read("BCD-Origin.csv",                   "ID",           "Name", mkAjaxLookup(ShowAll.Enabled))
  private val port                     = read("BCD-Port.csv",                     "PortId",       "Name", mkAjaxLookup(ShowAll.Disabled))
  private val transportMode            = read("BCD-TransportMode.csv",            "ID",           "Name", mkRadioLookup)
  private val intentBuyingWhat         = read("BCD-IntentBuyingWhat.csv",         "id",           "name", mkRadioLookup)
  private val intentBigPurchase        = read("BCD-IntentBigPurchase.csv",        "id",           "name", mkRadioLookup)
  private val intentLivingCostsAndFees = read("BCD-IntentLivingCostsAndFees.csv", "id",           "name", mkRadioLookup)
  private val intentBusiness           = read("BCD-IntentBusiness.csv",           "id",           "name", mkRadioLookup)
  private val intentOther              = read("BCD-IntentOther.csv",              "id",           "name", mkRadioLookup)
  private val originWho                = read("BCD-OriginWho.csv",                "id",           "name", mkRadioLookup)
  private val originSellingSomething   = read("BCD-OriginSellingSomething.csv",   "id",           "name", mkRadioLookup)
  private val originMainPart           = read("BCD-OriginMainPart.csv",           "id",           "name", mkRadioLookup)
  private val originSavingsEarnings    = read("BCD-OriginSavingsEarnings.csv",    "id",           "name", mkRadioLookup)
  // format: on

  private def mkAutocoplete(options: Map[LookupLabel, LookupId]): AutocompleteEngine[LookupRecord] = {

    val engine: AutocompleteEngine[LookupRecord] = new AutocompleteEngine.Builder[LookupRecord]()
      .setIndex(new LookupAdapter[LookupRecord]())
      .setAnalyzers(new LowerCaseTransformer(), new WordTokenizer())
      .build()

    options.keys.foreach(lv => engine.add(new LookupRecord(lv.label)))

    engine

  }

  val registerLookup: Map[Register, LookupType] =
    Map(
      Register.CashType                 -> cashType,
      Register.Country                  -> country,
      Register.Currency                 -> currency,
      Register.Intent                   -> intent,
      Register.Intercept                -> intercept,
      Register.Origin                   -> origin,
      Register.Port                     -> port,
      Register.TransportMode            -> transportMode,
      Register.OriginWho                -> originWho,
      Register.OriginMainPart           -> originMainPart,
      Register.OriginSavingsEarnings    -> originSavingsEarnings,
      Register.OriginSellingSomething   -> originSellingSomething,
      Register.IntentBuyingWhat         -> intentBuyingWhat,
      Register.IntentBusiness           -> intentBusiness,
      Register.IntentBigPurchase        -> intentBigPurchase,
      Register.IntentLivingCostsAndFees -> intentLivingCostsAndFees,
      Register.IntentOther              -> intentOther
    )
}
