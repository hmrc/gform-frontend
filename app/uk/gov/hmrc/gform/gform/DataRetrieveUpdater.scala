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

package uk.gov.hmrc.gform.gform

import uk.gov.hmrc.gform.sharedmodel.DataRetrieve
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.{ BankAccountInsights, BusinessBankAccountExistence, CompanyRegistrationNumber, Employments, NinoInsights, PersonalBankAccountExistence, PersonalBankAccountExistenceWithName, ValidateBankDetails }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

class DataRetrieveUpdater(dataRetrieve: DataRetrieve, index: Int, baseIds: List[FormComponentId]) {
  def update: DataRetrieve = dataRetrieve match {
    case ValidateBankDetails(id, sortCode, accountNumber, iff) =>
      val exprUpdater = new ExprUpdater(index, baseIds)
      ValidateBankDetails(
        id.withIndex(index),
        exprUpdater.expandExpr(sortCode),
        exprUpdater.expandExpr(accountNumber),
        iff
      )

    case BusinessBankAccountExistence(id, sortCode, accountNumber, companyName, iff) =>
      val exprUpdater = new ExprUpdater(index, baseIds)
      BusinessBankAccountExistence(
        id.withIndex(index),
        exprUpdater.expandExpr(sortCode),
        exprUpdater.expandExpr(accountNumber),
        exprUpdater.expandExpr(companyName),
        iff
      )

    case PersonalBankAccountExistence(id, sortCode, accountNumber, firstName, lastName, iff) =>
      val exprUpdater = new ExprUpdater(index, baseIds)
      PersonalBankAccountExistence(
        id.withIndex(index),
        exprUpdater.expandExpr(sortCode),
        exprUpdater.expandExpr(accountNumber),
        exprUpdater.expandExpr(firstName),
        exprUpdater.expandExpr(lastName),
        iff
      )
    case PersonalBankAccountExistenceWithName(id, sortCode, accountNumber, name, iff) =>
      val exprUpdater = new ExprUpdater(index, baseIds)
      PersonalBankAccountExistenceWithName(
        id.withIndex(index),
        exprUpdater.expandExpr(sortCode),
        exprUpdater.expandExpr(accountNumber),
        exprUpdater.expandExpr(name),
        iff
      )

    case CompanyRegistrationNumber(id, companyNumber, iff) =>
      val exprUpdater = new ExprUpdater(index, baseIds)
      CompanyRegistrationNumber(id.withIndex(index), exprUpdater.expandExpr(companyNumber), iff)

    case NinoInsights(id, nino, iff) =>
      val exprUpdater = new ExprUpdater(index, baseIds)
      NinoInsights(id.withIndex(index), exprUpdater.expandExpr(nino), iff)

    case BankAccountInsights(id, sortCode, accountNumber, iff) =>
      val exprUpdater = new ExprUpdater(index, baseIds)
      BankAccountInsights(
        id.withIndex(index),
        exprUpdater.expandExpr(sortCode),
        exprUpdater.expandExpr(accountNumber),
        iff
      )

    case Employments(id, nino, taxYear, iff) =>
      val exprUpdater = new ExprUpdater(index, baseIds)
      Employments(id.withIndex(index), exprUpdater.expandExpr(nino), exprUpdater.expandExpr(taxYear), iff)
  }
}

object DataRetrieveUpdater {
  def apply(dataRetrieve: DataRetrieve, index: Int, baseIds: List[FormComponentId]): DataRetrieve =
    new DataRetrieveUpdater(dataRetrieve, index, baseIds).update
}
