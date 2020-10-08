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
  implicit private val lookupKeyWordsCellDecoder: CellDecoder[LookupKeyWords] =
    implicitly[CellDecoder[Option[String]]].map(s => LookupKeyWords(s.map(_.trim)))
  implicit private val lookupCurrencyCountryCodeCellDecoder: CellDecoder[LookupCurrencyCountryCode] =
    implicitly[CellDecoder[String]].map(s => LookupCurrencyCountryCode(s.trim))
  implicit private val lookupPortTypeCellDecoder: CellDecoder[LookupPortType] =
    implicitly[CellDecoder[Int]].map(r => LookupPortType(r))

  private def read(
    filename: String,
    id: String,
    englishLabel: String,
    welshLabel: String,
    mkLookupType: LocalisedLookupOptions => LookupType): LookupType = {

    val headerDecoder: HeaderDecoder[(LookupLabel, LookupLabel, LookupId)] =
      HeaderDecoder.decoder(englishLabel, welshLabel, id)((_: LookupLabel, _: LookupLabel, _: LookupId))
    val lookup = getClass.getClassLoader.getResourceAsStream("lookup/" + filename)
    mkLookupType {
      val cvsData: List[(LookupLabel, LookupLabel, LookupId)] = lookup
        .unsafeReadCsv[List, (LookupLabel, LookupLabel, LookupId)](rfc.withHeader)(
          headerDecoder,
          implicitly,
          implicitly)

      val (enOptions, cyOptions): (List[(LookupLabel, LookupInfo)], List[(LookupLabel, LookupInfo)]) =
        cvsData.zipWithIndex.map {
          case ((enLabel, cyLabel, id), idx) =>
            val li = DefaultLookupInfo(id, idx)
            ((enLabel, li), (cyLabel, li))
        }.unzip

      LocalisedLookupOptions {
        Map(
          LangADT.En -> LookupOptions(enOptions.toMap),
          LangADT.Cy -> LookupOptions(cyOptions.toMap)
        )
      }
    }
  }

  private def readCountries(
    filename: String,
    id: String,
    englishLabel: String,
    welshLabel: String,
    keyWords: String,
    priority: String,
    region: String,
    mkLookupType: LocalisedLookupOptions => LookupType): LookupType = {

    val headerDecoder
      : HeaderDecoder[(LookupLabel, LookupLabel, LookupId, LookupKeyWords, LookupPriority, LookupRegion)] =
      HeaderDecoder.decoder(englishLabel, welshLabel, id, keyWords, priority, region)(
        (_: LookupLabel, _: LookupLabel, _: LookupId, _: LookupKeyWords, _: LookupPriority, _: LookupRegion))
    val lookup = getClass.getClassLoader.getResourceAsStream("lookup/" + filename)
    mkLookupType {
      val cvsData: List[(LookupLabel, LookupLabel, LookupId, LookupKeyWords, LookupPriority, LookupRegion)] = lookup
        .unsafeReadCsv[List, (LookupLabel, LookupLabel, LookupId, LookupKeyWords, LookupPriority, LookupRegion)](
          rfc.withHeader)(headerDecoder, implicitly, implicitly)

      val (enOptions, cyOptions): (List[(LookupLabel, LookupInfo)], List[(LookupLabel, LookupInfo)]) =
        cvsData.zipWithIndex.map {
          case ((enLabel, cyLabel, id, keyWords, priority, region), idx) =>
            val li = CountryLookupInfo(id, idx, keyWords, priority, region)
            ((enLabel, li), (cyLabel, li))
        }.unzip

      LocalisedLookupOptions {
        Map(
          LangADT.En -> LookupOptions(enOptions.toMap),
          LangADT.Cy -> LookupOptions(cyOptions.toMap)
        )
      }
    }
  }

  private def readCurrencies(
    filename: String,
    id: String,
    englishLabel: String,
    welshLabel: String,
    keyWords: String,
    priority: String,
    countryCode: String,
    mkLookupType: LocalisedLookupOptions => LookupType): LookupType = {

    val headerDecoder
      : HeaderDecoder[(LookupLabel, LookupLabel, LookupId, LookupKeyWords, LookupPriority, LookupCurrencyCountryCode)] =
      HeaderDecoder.decoder(englishLabel, welshLabel, id, keyWords, priority, countryCode)(
        (
          _: LookupLabel,
          _: LookupLabel,
          _: LookupId,
          _: LookupKeyWords,
          _: LookupPriority,
          _: LookupCurrencyCountryCode))
    val lookup = getClass.getClassLoader.getResourceAsStream("lookup/" + filename)
    mkLookupType {
      val cvsData
        : List[(LookupLabel, LookupLabel, LookupId, LookupKeyWords, LookupPriority, LookupCurrencyCountryCode)] =
        lookup
          .unsafeReadCsv[
            List,
            (LookupLabel, LookupLabel, LookupId, LookupKeyWords, LookupPriority, LookupCurrencyCountryCode)](
            rfc.withHeader)(headerDecoder, implicitly, implicitly)

      val (enOptions, cyOptions): (List[(LookupLabel, LookupInfo)], List[(LookupLabel, LookupInfo)]) =
        cvsData.zipWithIndex.map {
          case ((enLabel, cyLabel, id, keyWords, priority, countryCode), idx) =>
            val li = CurrencyLookupInfo(id, idx, keyWords, priority, countryCode)
            ((enLabel, li), (cyLabel, li))
        }.unzip

      LocalisedLookupOptions {
        Map(
          LangADT.En -> LookupOptions(enOptions.toMap),
          LangADT.Cy -> LookupOptions(cyOptions.toMap)
        )
      }
    }
  }

  private def readPorts(
    filename: String,
    id: String,
    englishLabel: String,
    welshLabel: String,
    keyWords: String,
    priority: String,
    region: String,
    portType: String,
    mkLookupType: LocalisedLookupOptions => LookupType): LookupType = {

    val headerDecoder: HeaderDecoder[(
      LookupLabel,
      LookupLabel,
      LookupId,
      LookupKeyWords,
      LookupPriority,
      LookupRegion,
      LookupPortType)] =
      HeaderDecoder.decoder(englishLabel, welshLabel, id, keyWords, priority, region, portType)(
        (
          _: LookupLabel,
          _: LookupLabel,
          _: LookupId,
          _: LookupKeyWords,
          _: LookupPriority,
          _: LookupRegion,
          _: LookupPortType))
    val lookup = getClass.getClassLoader.getResourceAsStream("lookup/" + filename)
    mkLookupType {
      val cvsData
        : List[(LookupLabel, LookupLabel, LookupId, LookupKeyWords, LookupPriority, LookupRegion, LookupPortType)] =
        lookup
          .unsafeReadCsv[
            List,
            (LookupLabel, LookupLabel, LookupId, LookupKeyWords, LookupPriority, LookupRegion, LookupPortType)](
            rfc.withHeader)(headerDecoder, implicitly, implicitly)

      val (enOptions, cyOptions): (List[(LookupLabel, LookupInfo)], List[(LookupLabel, LookupInfo)]) =
        cvsData.zipWithIndex.map {
          case ((enLabel, cyLabel, id, keyWords, priority, region, portType), idx) =>
            val li = PortLookupInfo(id, idx, keyWords, priority, region, portType)
            ((enLabel, li), (cyLabel, li))
        }.unzip

      LocalisedLookupOptions {
        Map(
          LangADT.En -> LookupOptions(enOptions.toMap),
          LangADT.Cy -> LookupOptions(cyOptions.toMap)
        )
      }
    }
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
            engine.add(new LookupRecord(ll.label, LookupPriority(1), LookupKeyWords(None)))
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
