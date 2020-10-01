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

package uk.gov.hmrc.gform.services

/* import uk.gov.hmrc.gform.Helpers.toSmartString
 * import uk.gov.hmrc.gform.Spec
 * import uk.gov.hmrc.gform.formtemplate.SectionSyntax
 * import uk.gov.hmrc.gform.graph.RecData
 * import uk.gov.hmrc.gform.keystore.RepeatingComponentService
 * import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT, VariadicFormData }
 * import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate._
 *
 * import scala.collection.immutable.List
 * import uk.gov.hmrc.http.HeaderCarrier
 *
 * class RepeatingComponentServiceSpec extends Spec with ExampleData {
 *
 *   // TODO JoVl - recreate these test in FormModelSpec.scala
 *
 *   private implicit lazy val hc = HeaderCarrier()
 *   private implicit val l = LangADT.En
 *
 *   "getAllSections" should "return only sections in template when no repeating sections are defined" in {
 *
 *     val formTemplate = super.formTemplate.copy(sections = List(`section - group`))
 *
 *     RepeatingComponentService.getAllSections(formTemplate, FormDataRecalculated.empty) shouldBe List(`section - group`)
 *   }
 *
 *   it should "return no dynamically created sections when field in repeatsMax expression in repeating group and no form data" in {
 *
 *     val formTemplate = super.formTemplate.copy(sections = List(`section - group`, `repeating section`))
 *
 *     val expectedList = List(`section - group`)
 *
 *     RepeatingComponentService.getAllSections(formTemplate, FormDataRecalculated.empty) shouldBe expectedList
 *   }
 *
 *   it should "return a dynamically created section when field to track in a NON-repeating group" in {
 *     val thisSection2 = `repeating section`
 *       .updateTitle(toSmartString("Repeating section title $n"))
 *       .updateShortName(Some(toSmartString("shortName $n")))
 *
 *     val formTemplate = super.formTemplate.copy(sections = List(`section - group`, thisSection2))
 *
 *     val textFieldDosR = `fieldValue - surname`.copy(id = FormComponentId(s"1_${`fieldId - surname`.value}"))
 *     val sectionR = thisSection2
 *       .updateTitle(toSmartString("Repeating section title 1"))
 *       .updateShortName(Some(toSmartString("shortName 1")))
 *       .updateFields(List(textFieldDosR))
 *
 *     val expectedList = List(`section - group`, sectionR)
 *
 *     val formData = mkFormDataRecalculated(VariadicFormData.ones(`fieldId - firstName` -> "1"))
 *
 *     RepeatingComponentService.getAllSections(formTemplate, formData) shouldBe expectedList
 *   }
 *
 *   it should "return dynamically created sections (title and shortName text built dynamically) when field to track in a NON-repeating group, with form data" in {
 *     val thisSection2 = `repeating section`
 *       .updateTitle(toSmartString("Repeating section title $n"))
 *       .updateShortName(Some(toSmartString("shortName $n")))
 *     val formTemplate = super.formTemplate.copy(sections = List(`section - group`, thisSection2))
 *
 *     val textFieldDos1 = `fieldValue - surname`.copy(id = FormComponentId(s"1_${`fieldId - surname`.value}"))
 *     val textFieldDos2 = `fieldValue - surname`.copy(id = FormComponentId(s"2_${`fieldId - surname`.value}"))
 *     val sectionR1 = thisSection2
 *       .updateFields(List(textFieldDos1))
 *       .updateTitle(toSmartString("Repeating section title 1"))
 *       .updateShortName(Some(toSmartString("shortName 1")))
 *
 *     val sectionR2 = thisSection2
 *       .updateFields(List(textFieldDos2))
 *       .updateTitle(toSmartString("Repeating section title 2"))
 *       .updateShortName(Some(toSmartString("shortName 2")))
 *
 *     val expectedList = List(`section - group`, sectionR1, sectionR2)
 *
 *     val formData = mkFormDataRecalculated(VariadicFormData.ones(`fieldId - firstName` -> "2"))
 *
 *     RepeatingComponentService.getAllSections(formTemplate, formData) shouldBe expectedList
 *   }
 *
 *   protected def mkFormDataRecalculated(data: VariadicFormData): FormDataRecalculated =
 *     FormDataRecalculated.empty.copy(recData = RecData.fromData(data))
 * } */
