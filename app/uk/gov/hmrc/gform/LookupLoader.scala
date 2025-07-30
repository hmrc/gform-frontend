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

package uk.gov.hmrc.gform

import java.nio.file.FileSystems
import kantan.csv._
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.similarities.BooleanSimilarity
import org.apache.lucene.store.NIOFSDirectory
import uk.gov.hmrc.gform.gform.csv.CsvUtils
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Register

class LookupLoader(indexPath: String) {

  implicit private val lookupIdCellDecoder: CellDecoder[LookupId] =
    implicitly[CellDecoder[String]].map(s => LookupId(s.trim))
  implicit private val lookupLabelCellDecoder: CellDecoder[LookupLabel] =
    implicitly[CellDecoder[String]].map(s => LookupLabel(s.trim))
  implicit private val lookupPriorityCellDecoder: CellDecoder[LookupPriority] =
    implicitly[CellDecoder[Option[Int]]].map(p => LookupPriority(p.getOrElse(1)))
  implicit private val lookupRegionCellDecoder: CellDecoder[LookupRegion] =
    implicitly[CellDecoder[String]].map(r => LookupRegion(r))
  implicit private val lookupCountryCodeCellDecoder: CellDecoder[LookupCountryCode] =
    implicitly[CellDecoder[String]].map(s => LookupCountryCode(s.trim))
  implicit private val lookupPortTypeCellDecoder: CellDecoder[LookupPortType] =
    implicitly[CellDecoder[String]].map(r => LookupPortType(r))
  implicit private val lookupPortCodeCellDecoder: CellDecoder[LookupPortCode] =
    implicitly[CellDecoder[String]].map(r => LookupPortCode(r))
  implicit private val lookupSicCodeSectionDecoder: CellDecoder[LookupSicCodeSection] =
    implicitly[CellDecoder[String]].map(r => LookupSicCodeSection(r))
  implicit private val lookupInGibraltarEuEeaEfta: CellDecoder[LookupInGibraltarEuEeaEfta] =
    implicitly[CellDecoder[String]].map(r => LookupInGibraltarEuEeaEfta(r))

  type LookupDetails = (LookupLabel, LookupInfo)

  private def getLocalisedLookupOptions[A](
    fileName: String,
    headerDecoder: HeaderDecoder[A],
    f: A => Int => (LookupDetails, LookupDetails)
  ): LocalisedLookupOptions = {

    val csvData: List[A] = CsvUtils.readCsv(fileName, headerDecoder)

    val (enOptions, cyOptions): (List[LookupDetails], List[LookupDetails]) =
      csvData.zipWithIndex.map { case (a, idx) =>
        f(a)(idx)
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
    mkLookupType: LocalisedLookupOptions => LookupType
  ): LookupType = {

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

  private def readNationalities(
    filename: String,
    idColumn: String,
    englishLabel: String,
    welshLabel: String,
    priority: String,
    priorityUk: String,
    region: String,
    countryCode: String,
    mkLookupType: LocalisedLookupOptions => LookupType
  ): LookupType = {

    type ColumnData =
      (
        LookupLabel,
        LookupLabel,
        LookupId,
        LookupPriority,
        LookupPriority,
        LookupRegion,
        LookupCountryCode
      )

    val headerDecoder: HeaderDecoder[ColumnData] =
      HeaderDecoder.decoder(
        englishLabel,
        welshLabel,
        idColumn,
        priority,
        priorityUk,
        region,
        countryCode
      )(
        (
          _: LookupLabel,
          _: LookupLabel,
          _: LookupId,
          _: LookupPriority,
          _: LookupPriority,
          _: LookupRegion,
          _: LookupCountryCode
        )
      )

    val csvWithColumns = CsvUtils.readCsvWithColumns(filename)
    def processData(columnData: ColumnData)(index: Int): (LookupDetails, LookupDetails) = {
      val (enLabel, cyLabel, id, priority, priorityUk, region, countryCode) = columnData

      val columns = csvWithColumns.find(_(idColumn) == id.id).get
      val li = NationalityLookupInfo(id, index, priority, priorityUk, region, countryCode, columns)
      ((enLabel, li), (cyLabel, li))
    }

    mkLookupType(getLocalisedLookupOptions(filename, headerDecoder, processData))
  }

  private def readCountries(
    filename: String,
    idColumnName: String,
    englishLabel: String,
    welshLabel: String,
    priority: String,
    priorityUk: String,
    region: String,
    inGibraltarEuEeaEfta: String,
    mkLookupType: LocalisedLookupOptions => LookupType
  ): LookupType = {

    type ColumnData =
      (
        LookupLabel,
        LookupLabel,
        LookupId,
        LookupPriority,
        LookupPriority,
        LookupRegion,
        LookupInGibraltarEuEeaEfta
      )

    val headerDecoder: HeaderDecoder[ColumnData] =
      HeaderDecoder.decoder(
        englishLabel,
        welshLabel,
        idColumnName,
        priority,
        priorityUk,
        region,
        inGibraltarEuEeaEfta
      )(
        (
          _: LookupLabel,
          _: LookupLabel,
          _: LookupId,
          _: LookupPriority,
          _: LookupPriority,
          _: LookupRegion,
          _: LookupInGibraltarEuEeaEfta
        )
      )

    val csvWithColumns = CsvUtils.readCsvWithColumns(filename)
    def processData(columnData: ColumnData)(index: Int): (LookupDetails, LookupDetails) = {
      val (enLabel, cyLabel, id, priority, priorityUk, region, inGibraltarEuEeaEfta) = columnData

      val columns = csvWithColumns.find(_.get(idColumnName).get == id.id).get
      val li = CountryLookupInfo(id, index, priority, priorityUk, region, inGibraltarEuEeaEfta, columns)
      ((enLabel, li), (cyLabel, li))
    }

    mkLookupType(getLocalisedLookupOptions(filename, headerDecoder, processData))
  }

  private def readFiveColumnFormat(
    filename: String,
    idColumnName: String,
    englishLabel: String,
    welshLabel: String,
    priority: String,
    mkLookupType: LocalisedLookupOptions => LookupType
  ): LookupType = {

    type ColumnData =
      (LookupLabel, LookupLabel, LookupId, LookupPriority)

    val headerDecoder: HeaderDecoder[ColumnData] =
      HeaderDecoder.decoder(englishLabel, welshLabel, idColumnName, priority)(
        (
          _: LookupLabel,
          _: LookupLabel,
          _: LookupId,
          _: LookupPriority
        )
      )

    val csvWithColumns = CsvUtils.readCsvWithColumns(filename)

    def processData(columnData: ColumnData)(index: Int): (LookupDetails, LookupDetails) = {
      val (enLabel, cyLabel, id, priority) = columnData

      val columns = csvWithColumns.find(_.get(idColumnName).get == id.id).get
      val li = FiveColumnLookupInfo(id, index, priority, columns)
      ((enLabel, li), (cyLabel, li))
    }

    mkLookupType(getLocalisedLookupOptions(filename, headerDecoder, processData))
  }

  private def readCurrencies(
    filename: String,
    id: String,
    englishLabel: String,
    welshLabel: String,
    priority: String,
    countryCode: String,
    mkLookupType: LocalisedLookupOptions => LookupType
  ): LookupType = {

    type ColumnData = (LookupLabel, LookupLabel, LookupId, LookupPriority, LookupCountryCode)

    val headerDecoder: HeaderDecoder[ColumnData] =
      HeaderDecoder.decoder(englishLabel, welshLabel, id, priority, countryCode)(
        (_: LookupLabel, _: LookupLabel, _: LookupId, _: LookupPriority, _: LookupCountryCode)
      )

    def processData(columnData: ColumnData)(index: Int): (LookupDetails, LookupDetails) = {
      val (enLabel, cyLabel, id, priority, countryCode) = columnData
      val li = CurrencyLookupInfo(id, index, priority, countryCode)
      ((enLabel, li), (cyLabel, li))
    }

    mkLookupType(getLocalisedLookupOptions(filename, headerDecoder, processData))
  }

  private def readPorts(
    filename: String,
    id: String,
    englishLabel: String,
    welshLabel: String,
    priority: String,
    region: String,
    portType: String,
    countryCode: String,
    portCode: String,
    mkLookupType: LocalisedLookupOptions => LookupType
  ): LookupType = {

    type ColumnData = (
      LookupLabel,
      LookupLabel,
      LookupId,
      LookupPriority,
      LookupRegion,
      LookupPortType,
      LookupCountryCode,
      LookupPortCode
    )

    val headerDecoder: HeaderDecoder[ColumnData] =
      HeaderDecoder.decoder(englishLabel, welshLabel, id, priority, region, portType, countryCode, portCode)(
        (
          _: LookupLabel,
          _: LookupLabel,
          _: LookupId,
          _: LookupPriority,
          _: LookupRegion,
          _: LookupPortType,
          _: LookupCountryCode,
          _: LookupPortCode
        )
      )

    def processData(columnData: ColumnData)(index: Int): (LookupDetails, LookupDetails) = {
      val (enLabel, cyLabel, id, priority, region, portType, countryCode, portCode) = columnData
      val li = PortLookupInfo(id, index, priority, region, portType, countryCode, portCode)
      ((enLabel, li), (cyLabel, li))
    }

    mkLookupType(getLocalisedLookupOptions(filename, headerDecoder, processData))
  }

  private def readSicCode(
    filename: String,
    idColumnName: String,
    englishLabel: String,
    welshLabel: String,
    section: String,
    mkLookupType: LocalisedLookupOptions => LookupType
  ): LookupType = {

    type ColumnData = (LookupLabel, LookupLabel, LookupId, LookupSicCodeSection)

    val headerDecoder: HeaderDecoder[ColumnData] =
      HeaderDecoder.decoder(englishLabel, welshLabel, idColumnName, section)(
        (_: LookupLabel, _: LookupLabel, _: LookupId, _: LookupSicCodeSection)
      )

    val csvWithColumns = CsvUtils.readCsvWithColumns(filename)
    def processData(columnData: ColumnData)(index: Int): (LookupDetails, LookupDetails) = {
      val (enLabel, cyLabel, id, sicCodeSection) = columnData

      val columns = csvWithColumns.find(_(idColumnName) == id.id).get
      val li = SicCodeLookupInfo(id, index, sicCodeSection, columns)
      ((enLabel, li), (cyLabel, li))
    }

    mkLookupType(getLocalisedLookupOptions(filename, headerDecoder, processData))
  }

  private def readAgentComplaintCategories(
    filename: String,
    idColumnName: String,
    englishLabel: String,
    welshLabel: String,
    mkLookupType: LocalisedLookupOptions => LookupType
  ): LookupType = {

    type ColumnData =
      (LookupLabel, LookupLabel, LookupId)

    val headerDecoder: HeaderDecoder[ColumnData] =
      HeaderDecoder.decoder(englishLabel, welshLabel, idColumnName)(
        (
          _: LookupLabel,
          _: LookupLabel,
          _: LookupId
        )
      )

    val csvWithColumns = CsvUtils.readCsvWithColumns(filename)
    def processData(columnData: ColumnData)(index: Int): (LookupDetails, LookupDetails) = {
      val (enLabel, cyLabel, id) = columnData

      val columns = csvWithColumns.find(_.get(idColumnName).get == id.id).get
      val li = AgentComplaintCategoriesLookupInfo(id, index, columns)
      ((enLabel, li), (cyLabel, li))
    }

    mkLookupType(getLocalisedLookupOptions(filename, headerDecoder, processData))
  }

  def mkIndexSearcher(indexName: String): IndexSearcher = {
    def index(indexName: String): NIOFSDirectory = {
      val path = FileSystems.getDefault.getPath(indexPath, indexName)
      new NIOFSDirectory(path)
    }

    val reader: DirectoryReader = DirectoryReader.open(index(indexName))
    val searcher = new IndexSearcher(reader)

    searcher.setSimilarity(new BooleanSimilarity())

    searcher

  }

  private def mkAjaxLookup(showAll: ShowAll, indexName: String)(m: LocalisedLookupOptions): AjaxLookup =
    AjaxLookup(m, mkIndexSearcher(indexName), showAll)

  private def mkRadioLookup(m: LocalisedLookupOptions): RadioLookup = RadioLookup(m)

  // format: off
  private val cashType                 = read("BCD-CashType.csv",                 "Id", "En",   "Cy",   mkRadioLookup)
  private val intent                   = read("BCD-Intent.csv",                   "Id", "En",   "Cy",   mkRadioLookup)
  private val intercept                = read("BCD-Intercept.csv",                "Id", "Name", "Name", mkRadioLookup)
  private val transportMode            = read("BCD-TransportMode.csv",            "Id", "Name", "Name", mkRadioLookup)
  private val intentBuyingWhat         = read("BCD-IntentBuyingWhat.csv",         "Id", "En",   "Cy",   mkRadioLookup)
  private val intentBigPurchase        = read("BCD-IntentBigPurchase.csv",        "Id", "En",   "Cy",   mkRadioLookup)
  private val intentLivingCostsAndFees = read("BCD-IntentLivingCostsAndFees.csv", "Id", "En",   "Cy",   mkRadioLookup)
  private val intentBusiness           = read("BCD-IntentBusiness.csv",           "Id", "En",   "Cy",   mkRadioLookup)
  private val intentOther              = read("BCD-IntentOther.csv",              "Id", "En",   "Cy",   mkRadioLookup)
  private val originWho                = read("BCD-OriginWho.csv",                "Id", "En",   "Cy",   mkRadioLookup)
  private val originSellingSomething   = read("BCD-OriginSellingSomething.csv",   "Id", "En",   "Cy",   mkRadioLookup)
  private val originMainPart           = read("BCD-OriginMainPart.csv",           "Id", "En",   "Cy",   mkRadioLookup)
  private val originSavingsEarnings    = read("BCD-OriginSavingsEarnings.csv",    "Id", "En",   "Cy",   mkRadioLookup)
  private val origin                   = read("BCD-Origin.csv",                   "Id", "En",   "Cy",   mkAjaxLookup(ShowAll.Enabled, "origin"))
  private val agentComplaintCategories = readAgentComplaintCategories("BCD-AgentComplaintCategories.csv", "Code", "Name", "Name-cy",  mkAjaxLookup(ShowAll.Enabled, "agent-complaint-categories"))
  private val country                  = readCountries("BCD-Country.csv",         "CountryCode",   "Name", "Name-cy", "Priority", "PriorityUK", "Region", "InGibraltarEuEeaEfta", mkAjaxLookup(ShowAll.Enabled, "country"))
  private val nationality              = readNationalities("BCD-Nationality.csv", "NationalityId", "Name", "Name-cy", "Priority", "PriorityUK", "Region", "CountryCode",          mkAjaxLookup(ShowAll.Enabled, "nationality"))
  private val currency                 = readCurrencies("BCD-Currency.csv",       "CurrencyCode",  "Name", "Name-cy", "Priority", "CountryCode", mkAjaxLookup(ShowAll.Disabled, "currency"))
  private val port                     = readPorts("BCD-Port.csv",                "PortCode",      "Name", "Name-cy", "Priority", "Region", "PortType", "CountryCode", "PortCode", mkAjaxLookup(ShowAll.Disabled, "port"))
  private val sicCode                  = readSicCode("SicCode.csv",               "SicCode",       "Name", "Name-cy", "Section", mkAjaxLookup(ShowAll.Disabled, "sic-code"))
  private val sdltReliefType           = readFiveColumnFormat("SDLT-TypeOfRelief.csv",   "Code", "ReliefType",     "ReliefType-cy",     "Priority", mkAjaxLookup(ShowAll.Enabled, "type-of-relief"))
  private val localAuthority           = readFiveColumnFormat("BCD-LocalAuthority.csv",  "Code", "LocalAuthority", "LocalAuthority-cy", "Priority", mkAjaxLookup(ShowAll.Enabled, "local-authority"))
  private val vfrsTradeSector          = readFiveColumnFormat("BCD-VfrsTradeSector.csv", "Code", "TradeSector",    "TradeSector-cy",    "Priority", mkAjaxLookup(ShowAll.Enabled, "vfrs-trade-sector"))
  // format: on

  val registerLookup: Map[Register, LookupType] =
    Map(
      Register.AgentComplaintCategories -> agentComplaintCategories,
      Register.CashType                 -> cashType,
      Register.Country                  -> country,
      Register.Nationality              -> nationality,
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
      Register.IntentOther              -> intentOther,
      Register.SicCode                  -> sicCode,
      Register.SdltReliefType           -> sdltReliefType,
      Register.LocalAuthority           -> localAuthority,
      Register.VfrsTradeSector          -> vfrsTradeSector
    )
}
