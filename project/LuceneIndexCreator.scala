/*
 * Copyright 2017 HM Revenue & Customs
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

import com.typesafe.sbt.SbtNativePackager.Universal
import com.typesafe.sbt.packager.MappingsHelper.contentOf
import java.util.regex.Pattern
import org.apache.lucene.analysis.core.WhitespaceTokenizer
import org.apache.lucene.analysis.{ Analyzer, TokenStream }
import org.apache.lucene.analysis.core.LowerCaseFilter
import org.apache.lucene.document.{ Document, Field, StoredField, TextField }
import org.apache.lucene.index.{ IndexWriter, IndexWriterConfig }
import org.apache.lucene.store.NIOFSDirectory
import sbt.Keys._
import sbt.{ Append, Compile, ConsoleLogger, Def, File, TaskKey, fileToRichFile }
import com.github.tototoshi.csv._

object LuceneIndexCreator {

  val log = ConsoleLogger()
  val indexBuild = TaskKey[Seq[File]]("index-build")
  val indexRebuild = TaskKey[Seq[File]]("index-rebuild") // Useful for development

  import Append._

  def indexBuildTask(rebuild: Boolean) = Def.task {
    val root = (Compile / resourceManaged).value / "conf" / "index"

    val metadata = List(
      // format: off
      CsvDefinition("BCD-Country.csv",                  "Name",           "Name-cy",           Some("KeyWords"), Some("KeyWords-cy"), "country"),
      CsvDefinition("BCD-Origin.csv",                   "En",             "Cy",                None,             None,                "origin"),
      CsvDefinition("BCD-AgentComplaintCategories.csv", "Name",           "Name-cy",           Some("KeyWords"), Some("KeyWords-cy"), "agent-complaint-categories"),
      CsvDefinition("BCD-Currency.csv",                 "Name",           "Name-cy",           Some("KeyWords"), Some("KeyWords-cy"), "currency"),
      CsvDefinition("BCD-Nationality.csv",              "Name",           "Name-cy",           Some("Keywords"), Some("Keywords-cy"), "nationality"),
      CsvDefinition("SicCode.csv",                      "Name",           "Name-cy",           None,             None,                "sic-code"),
      CsvDefinition("BCD-Port.csv",                     "Name",           "Name-cy",           Some("KeyWords"), Some("KeyWords"),    "port"),
      CsvDefinition("SDLT-TypeOfRelief.csv",            "ReliefType",     "ReliefType-cy",     Some("Keywords"), Some("Keywords"),    "type-of-relief"),
      CsvDefinition("BCD-LocalAuthority.csv",           "LocalAuthority", "LocalAuthority-cy", None,             None,                "local-authority"),
      CsvDefinition("BCD-VfrsTradeSector.csv",          "TradeSector",    "TradeSector-cy",    None,             None,                "vfrs-trade-sector"),
      // format: on
    )

    val files =
      metadata
        .map { csvDefinition =>
          IndexBuilder.buildIndex(root, csvDefinition, rebuild)
        }
        .flatMap { location =>
          // convert the location into a Sequence of files to be packaged
          Seq(location) ++ location.listFiles()
        }

    log.debug(s"Index files to be copied by resource generator ${files.mkString(",")}")
    files
  }

  val indexSettings = Seq(
    indexBuild := indexBuildTask(false).value,
    indexRebuild := indexBuildTask(true).value,
    Compile / resourceGenerators += indexBuildTask(false),
    Universal / mappings ++= contentOf((Compile / resourceManaged).value),
    // clean the old location where indexes were stored
    cleanFiles += baseDirectory(base => base / "conf" / "index").value
  )
}

case class CsvDefinition(
  file: String,
  contentEnHeader: String,
  contentCyHeader: String,
  keywordsEnHeader: Option[String],
  keywordsCyHeader: Option[String],
  indexName: String
)

case class CsvDocument(
  descriptionEn: String,
  descriptionCy: String,
  keywordsEn: Option[String],
  keywordsCy: Option[String]
)

class WhiteSpaceOnlyAnalyzer() extends Analyzer {
  override def createComponents(fieldName: String): Analyzer.TokenStreamComponents = {
    val tokenizer = new WhitespaceTokenizer() // keep -, €, $ etc.
    val lowercased: TokenStream = new LowerCaseFilter(tokenizer)
    new Analyzer.TokenStreamComponents(tokenizer, lowercased)
  }
}

object IndexBuilder {

  val FIELD_SEARCH_TERMS_EN = "searchTermsEn"
  val FIELD_SEARCH_TERMS_CY = "searchTermsCy"

  def produceDocuments(
    rows: List[Map[String, String]],
    addDocument: CsvDocument => Unit,
    csvDefinition: CsvDefinition
  ): Unit =
    rows.foreach { line =>
      val desc = line(csvDefinition.contentEnHeader)
      val descCy = line(csvDefinition.contentCyHeader)
      val maybeEnKeywords = csvDefinition.keywordsEnHeader.map(line(_))
      val maybeCyKeywords = csvDefinition.keywordsCyHeader.map(line(_))
      addDocument(CsvDocument(desc, descCy, maybeEnKeywords, maybeCyKeywords))
    }

  val log = ConsoleLogger()

  def buildIndex(rootPath: File, csvDefinition: CsvDefinition, rebuild: Boolean): File = {

    val csvFilename = csvDefinition.file
    val indexName = csvDefinition.indexName

    val indexFile = rootPath / indexName
    val indexPath = indexFile.toPath

    val index = new NIOFSDirectory(indexPath)

    if (rebuild) {
      index.listAll().foreach { fileName =>
        index.deleteFile(fileName)
        log.info(s"""Deleted existing index "$indexName" file: $fileName""")
      }
    } else {}

    if (index.listAll().isEmpty) {
      log.info(s"""$csvFilename - building new index "$indexName" into ${indexPath.toAbsolutePath}""")

      val startTime = System.currentTimeMillis()

      val reader = CSVReader.open("conf/lookup/" + csvFilename)

      val rows: List[Map[String, String]] = reader.allWithHeaders().map(_.map { case (k, v) => (k.trim, v.trim) })

      reader.close()

      val analyzer: Analyzer = new WhiteSpaceOnlyAnalyzer()

      val config = new IndexWriterConfig(analyzer)

      val w = new IndexWriter(index, config)

      produceDocuments(rows, addDoc(w), csvDefinition)

      val numIndexDocs = w.getDocStats().numDocs

      w.commit() // flush the contents
      w.close()

      log.info(
        s"""$csvFilename - index "$indexName" successfully built with $numIndexDocs docs in it (adding took ${System.currentTimeMillis - startTime}ms)."""
      )
    }

    indexFile
  }

  def addDoc(w: IndexWriter)(csvDoc: CsvDocument): Unit = {
    val doc = new Document

    doc.add(new TextField(FIELD_SEARCH_TERMS_EN, csvDoc.descriptionEn, Field.Store.YES))
    doc.add(new TextField(FIELD_SEARCH_TERMS_CY, csvDoc.descriptionCy, Field.Store.YES))

    csvDoc.keywordsEn.foreach { keywords =>
      doc.add(new TextField(FIELD_SEARCH_TERMS_EN, keywords, Field.Store.NO))
    }

    csvDoc.keywordsCy.foreach { keywords =>
      doc.add(new TextField(FIELD_SEARCH_TERMS_CY, keywords, Field.Store.NO))
    }

    w.addDocument(doc)
  }

}
