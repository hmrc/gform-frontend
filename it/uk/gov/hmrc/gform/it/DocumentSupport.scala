package uk.gov.hmrc.gform.it

import org.jsoup.nodes.Document

trait DocumentSupport {
  implicit class DocumentOps(document: Document) {
    def fieldValue(fieldName: String): String =
      document.getElementsByAttributeValue("name", fieldName).get(0).`val`()
  }
}
