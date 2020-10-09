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

package uk.gov.hmrc.gform

import com.miguelfonseca.completely.AutocompleteEngine
import com.miguelfonseca.completely.text.analyze.transform.LowerCaseTransformer
import kantan.csv._
import kantan.csv.ops._
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Register
import uk.gov.hmrc.gform.lookup._

class LookupLoader {

  implicit private val lookupIdCellDecoder: CellDecoder[LookupId] =
    implicitly[CellDecoder[String]].map(s => LookupId(s.trim))
  implicit private val lookupLabelCellDecoder: CellDecoder[LookupLabel] =
    implicitly[CellDecoder[String]].map(s => LookupLabel(s.trim))
  implicit private val lookupPriorityCellDecoder: CellDecoder[LookupPriority] =
    implicitly[CellDecoder[Option[Int]]].map(p => LookupPriority(p.getOrElse(1)))
  implicit private val lookupRegionCellDecoder: CellDecoder[LookupRegion] =
    implicitly[CellDecoder[Int]].map(r => LookupRegion(r))
  implicit private val lookupKeywordsCellDecoder: CellDecoder[LookupKeywords] =
    implicitly[CellDecoder[Option[String]]].map(s => LookupKeywords(s.map(_.trim)))
  implicit private val lookupCurrencyCountryCodeCellDecoder: CellDecoder[LookupCurrencyCountryCode] =
    implicitly[CellDecoder[String]].map(s => LookupCurrencyCountryCode(s.trim))
  implicit private val lookupPortTypeCellDecoder: CellDecoder[LookupPortType] =
    implicitly[CellDecoder[Int]].map(r => LookupPortType(r))

  type LookupDetails = (LookupLabel, LookupInfo)

  private def readCsv[A](filename: String, headerDecoder: HeaderDecoder[A]): List[A] = {
    val lookup = getClass.getClassLoader.getResourceAsStream("lookup/" + filename)

    lookup.unsafeReadCsv[List, A](rfc.withHeader)(headerDecoder, implicitly, implicitly)
  }

  private def getLocalisedLookupOptions[A](
    fileName: String,
    headerDecoder: HeaderDecoder[A],
    f: A => Int => (LookupDetails, LookupDetails)): LocalisedLookupOptions = {

    val csvData: List[A] = readCsv(fileName, headerDecoder)

    val (enOptions, cyOptions): (List[LookupDetails], List[LookupDetails]) =
      csvData.zipWithIndex.map {
        case (a, idx) => f(a)(idx)
      }.unzip

    LocalisedLookupOptions {
      Map(
        LangADT.En -> LookupOptions(enOptions.toMap),
        LangADT.Cy -> LookupOptions(cyOptions.toMap)
      )
    }
  }

  private def read(
    filename: String,
    id: String,
    englishLabel: String,
    welshLabel: String,
    mkLookupType: LocalisedLookupOptions => LookupType): LookupType = {

    type ColumnData = (LookupLabel, LookupLabel, LookupId)

    val headerDecoder: HeaderDecoder[ColumnData] =
      HeaderDecoder.decoder(englishLabel, welshLabel, id)((_: LookupLabel, _: LookupLabel, _: LookupId))

    def processData(columnData: ColumnData)(index: Int): (LookupDetails, LookupDetails) = {
      val (enLabel, cyLabel, id) = columnData
      val li = DefaultLookupInfo(id, index)
      ((enLabel, li), (cyLabel, li))
    }

    mkLookupType(getLocalisedLookupOptions(filename, headerDecoder, processData))
  }

  private def readCountries(
    filename: String,
    id: String,
    englishLabel: String,
    welshLabel: String,
    keywords: String,
    priority: String,
    region: String,
    mkLookupType: LocalisedLookupOptions => LookupType): LookupType = {

    type ColumnData = (LookupLabel, LookupLabel, LookupId, LookupKeywords, LookupPriority, LookupRegion)

    val headerDecoder: HeaderDecoder[ColumnData] =
      HeaderDecoder.decoder(englishLabel, welshLabel, id, keywords, priority, region)(
        (_: LookupLabel, _: LookupLabel, _: LookupId, _: LookupKeywords, _: LookupPriority, _: LookupRegion))

    def processData(columnData: ColumnData)(index: Int): (LookupDetails, LookupDetails) = {
      val (enLabel, cyLabel, id, keywords, priority, region) = columnData
      val li = CountryLookupInfo(id, index, keywords, priority, region)
      ((enLabel, li), (cyLabel, li))
    }

    mkLookupType(getLocalisedLookupOptions(filename, headerDecoder, processData))
  }

  private def readCurrencies(
    filename: String,
    id: String,
    englishLabel: String,
    welshLabel: String,
    keywords: String,
    priority: String,
    countryCode: String,
    mkLookupType: LocalisedLookupOptions => LookupType): LookupType = {

    type ColumnData = (LookupLabel, LookupLabel, LookupId, LookupKeywords, LookupPriority, LookupCurrencyCountryCode)

    val headerDecoder: HeaderDecoder[ColumnData] =
      HeaderDecoder.decoder(englishLabel, welshLabel, id, keywords, priority, countryCode)(
        (
          _: LookupLabel,
          _: LookupLabel,
          _: LookupId,
          _: LookupKeywords,
          _: LookupPriority,
          _: LookupCurrencyCountryCode))

    def processData(columnData: ColumnData)(index: Int): (LookupDetails, LookupDetails) = {
      val (enLabel, cyLabel, id, keywords, priority, countryCode) = columnData
      val li = CurrencyLookupInfo(id, index, keywords, priority, countryCode)
      ((enLabel, li), (cyLabel, li))
    }

    mkLookupType(getLocalisedLookupOptions(filename, headerDecoder, processData))
  }

  private def readPorts(
    filename: String,
    id: String,
    englishLabel: String,
    welshLabel: String,
    keywords: String,
    priority: String,
    region: String,
    portType: String,
    mkLookupType: LocalisedLookupOptions => LookupType): LookupType = {

    type ColumnData = (LookupLabel, LookupLabel, LookupId, LookupKeywords, LookupPriority, LookupRegion, LookupPortType)

    val headerDecoder: HeaderDecoder[ColumnData] =
      HeaderDecoder.decoder(englishLabel, welshLabel, id, keywords, priority, region, portType)(
        (
          _: LookupLabel,
          _: LookupLabel,
          _: LookupId,
          _: LookupKeywords,
          _: LookupPriority,
          _: LookupRegion,
          _: LookupPortType))

    def processData(columnData: ColumnData)(index: Int): (LookupDetails, LookupDetails) = {
      val (enLabel, cyLabel, id, keywords, priority, region, portType) = columnData
      val li = PortLookupInfo(id, index, keywords, priority, region, portType)
      ((enLabel, li), (cyLabel, li))
    }

    mkLookupType(getLocalisedLookupOptions(filename, headerDecoder, processData))
  }

  private def mkAjaxLookup(showAll: ShowAll)(m: LocalisedLookupOptions): AjaxLookup =
    AjaxLookup(m, mkAutocomplete(m), showAll)
  private def mkRadioLookup(m: LocalisedLookupOptions): RadioLookup = RadioLookup(m)

  // format: off
  private val cashType                 = read("BCD-CashType.csv",                 "ID",           "en",   "cy",   mkRadioLookup)
  private val intent                   = read("BCD-Intent.csv",                   "ID",           "en",   "cy",   mkRadioLookup)
  private val intercept                = read("BCD-Intercept.csv",                "ID",           "Name", "Name", mkRadioLookup)
  private val transportMode            = read("BCD-TransportMode.csv",            "ID",           "Name", "Name", mkRadioLookup)
  private val intentBuyingWhat         = read("BCD-IntentBuyingWhat.csv",         "id",           "en",   "cy",   mkRadioLookup)
  private val intentBigPurchase        = read("BCD-IntentBigPurchase.csv",        "id",           "en",   "cy",   mkRadioLookup)
  private val intentLivingCostsAndFees = read("BCD-IntentLivingCostsAndFees.csv", "id",           "en",   "cy",   mkRadioLookup)
  private val intentBusiness           = read("BCD-IntentBusiness.csv",           "id",           "en",   "cy",   mkRadioLookup)
  private val intentOther              = read("BCD-IntentOther.csv",              "id",           "en",   "cy",   mkRadioLookup)
  private val originWho                = read("BCD-OriginWho.csv",                "id",           "en",   "cy",   mkRadioLookup)
  private val originSellingSomething   = read("BCD-OriginSellingSomething.csv",   "id",           "en",   "cy",   mkRadioLookup)
  private val originMainPart           = read("BCD-OriginMainPart.csv",           "id",           "en",   "cy",   mkRadioLookup)
  private val originSavingsEarnings    = read("BCD-OriginSavingsEarnings.csv",    "id",           "en",   "cy",   mkRadioLookup)
  private val origin                   = read("BCD-Origin.csv",                   "ID",           "en",   "cy",   mkAjaxLookup(ShowAll.Enabled))
  private val country                  = readCountries("BCD-Country.csv",         "CountryCode",  "Name", "Name-cy", "KeyWords", "Priority", "Region", mkAjaxLookup(ShowAll.Disabled))
  private val currency                 = readCurrencies("BCD-Currency.csv",       "CurrencyCode", "Name", "Name-cy", "KeyWords", "Priority", "CountryCode", mkAjaxLookup(ShowAll.Disabled))
  private val port                     = readPorts("BCD-Port.csv",                "PortCode",     "Name", "Name",    "KeyWords", "Priority", "Region", "PortType",  mkAjaxLookup(ShowAll.Disabled))
  // format: on

  private def mkAutocomplete(options: LocalisedLookupOptions): Map[LangADT, AutocompleteEngine[LookupRecord]] =
    options.m.map {
      case (l, m) =>
        val engine: AutocompleteEngine[LookupRecord] = new AutocompleteEngine.Builder[LookupRecord]()
          .setIndex(new LookupAdapter[LookupRecord]())
          .setAnalyzers(new LowerCaseTransformer(), new CustomWordTokenizer())
          .build()

        m.options map {
          case (ll, DefaultLookupInfo(_, _)) =>
            engine.add(new LookupRecord(ll.label, LookupPriority(1), LookupKeywords(None)))
          case (ll, CountryLookupInfo(_, _, k, p, _))  => engine.add(new LookupRecord(ll.label, p, k))
          case (ll, CurrencyLookupInfo(_, _, k, p, _)) => engine.add(new LookupRecord(ll.label, p, k))
          case (ll, PortLookupInfo(_, _, k, p, _, _))  => engine.add(new LookupRecord(ll.label, p, k))
        }

        l -> engine
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
