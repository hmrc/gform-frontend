/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.{ BusinessBankAccountExistence, ValidateBankDetails }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

class DataRetrieveUpdater(dataRetrieve: DataRetrieve, index: Int, baseIds: List[FormComponentId]) {
  def update: DataRetrieve = dataRetrieve match {
    case ValidateBankDetails(id, sortCode, accountNumber) =>
      val exprUpdater = new ExprUpdater(index, baseIds)
      ValidateBankDetails(id.withIndex(index), exprUpdater.expandExpr(sortCode), exprUpdater.expandExpr(accountNumber))

    case BusinessBankAccountExistence(id, sortCode, accountNumber, companyName) =>
      val exprUpdater = new ExprUpdater(index, baseIds)
      BusinessBankAccountExistence(
        id.withIndex(index),
        exprUpdater.expandExpr(sortCode),
        exprUpdater.expandExpr(accountNumber),
        exprUpdater.expandExpr(companyName)
      )
  }
}

object DataRetrieveUpdater {
  def apply(dataRetrieve: DataRetrieve, index: Int, baseIds: List[FormComponentId]): DataRetrieve =
    new DataRetrieveUpdater(dataRetrieve, index, baseIds).update
}
