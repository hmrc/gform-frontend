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

package uk.gov.hmrc.gform.addresslookup

import cats.data.NonEmptyList
import cats.implicits._
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.{ Action, AnyContent, Call, MessagesControllerComponents, Request, Result }
import play.twirl.api.Html

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions }
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluationSyntax, SmartStringEvaluator }
import uk.gov.hmrc.gform.objectStore.{ Envelope, EnvelopeWithMapping }
import uk.gov.hmrc.gform.gform.{ Errors, FastForwardService }
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.lookup.LocalisedLookupOptions
import uk.gov.hmrc.gform.models.{ Basic, Bracket, DataExpanded, FastForward, FormModel, FormModelBuilder, SectionSelectorType, Visibility }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.monoidHtml
import uk.gov.hmrc.gform.sharedmodel.form.{ FormComponentIdToFileIdMapping, FormData, FormModelOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Address, Expr, FormComponent, FormComponentId, FormKind, FormTemplateContext, FormTemplateId, IsPostcodeLookup, Page, PostcodeLookup, Section, SectionNumber, SectionTitle4Ga, SuppressErrors }
import uk.gov.hmrc.gform.sharedmodel.{ LocalisedString, SmartString }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT }
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService }
import uk.gov.hmrc.gform.views.html.addresslookup
import uk.gov.hmrc.gform.views.html.hardcoded.pages.br
import uk.gov.hmrc.govukfrontend.views.Aliases.{ ErrorLink, ErrorMessage, ErrorSummary }
import uk.gov.hmrc.govukfrontend.views.html.components
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.http.{ HeaderCarrier, NotFoundException }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

class AddressLookupController(
  auth: AuthenticatedRequestActions,
  addressLookupService: AddressLookupService[Future],
  frontendAppConfig: FrontendAppConfig,
  i18nSupport: I18nSupport,
  messagesControllerComponents: MessagesControllerComponents,
  recalculation: Recalculation[Future, Throwable],
  formControllerRequestHandler: FormControllerRequestHandler,
  validationService: ValidationService,
  fastForwardService: FastForwardService,
  lookupOptions: LocalisedLookupOptions
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  import i18nSupport._

  def chooseAddress(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    fastForward: List[FastForward]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        val maybeAddressLookupResult: Option[AddressLookupResult] =
          cache.form.thirdPartyData.postcodeLookup.flatMap(_.get(formComponentId))
        maybeAddressLookupResult
          .fold(
            Future.failed[Result](
              new NotFoundException(s"No addresslookup found for FormComponentId: ${formComponentId.value}.")
            )
          ) { addressLookupResult =>
            addressLookupResult.response.addresses
              .fold {
                val renderComeBackLater =
                  cache.retrievals.renderSaveAndComeBackLater && !cache.formTemplate.draftRetrievalMethod.isNotPermitted
                Ok(
                  addresslookup.no_address_found(
                    cache.formTemplate,
                    frontendAppConfig,
                    addressLookupResult.request.postcode,
                    routes.AddressLookupController
                      .tryDifferentAddress(
                        cache.formTemplate._id,
                        maybeAccessCode,
                        formComponentId,
                        sectionNumber,
                        addressLookupResult.request.postcode,
                        fastForward
                      ),
                    goToSectionNumberLink(cache, formModelOptics, sectionNumber, maybeAccessCode, fastForward),
                    renderComeBackLater,
                    maybeAccessCode,
                    sectionNumber
                  )
                ).pure[Future]
              } { addressRecords =>
                if (addressRecords.size === 1) {
                  val addressId = addressRecords.head.id
                  addressLookupService
                    .saveAddress(cache.form, maybeAccessCode, formComponentId, addressId)
                    .as(
                      Redirect(
                        routes.AddressLookupController
                          .confirmAddress(
                            formTemplateId,
                            maybeAccessCode,
                            formComponentId,
                            sectionNumber,
                            fastForward
                          )
                      )
                    )
                } else {
                  val formModel = formModelOptics.formModelRenderPageOptics.formModel
                  val title = titleForChooseAddressPage(formModel, addressLookupResult, formComponentId)
                  Ok(
                    renderChooseAddressPage(
                      formComponentId,
                      addressIdForm,
                      addressRecords,
                      cache,
                      formModelOptics,
                      sectionNumber,
                      maybeAccessCode,
                      addressLookupResult,
                      title,
                      fastForward
                    )
                  ).pure[Future]
                }
              }
          }
    }

  private val addressIdForm: play.api.data.Form[String] = play.api.data.Form(
    play.api.data.Forms.single(
      "addressId" -> play.api.data.Forms.nonEmptyText
    )
  )

  private def goToSectionNumberLink(
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    sectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode],
    fastForward: List[FastForward]
  )(implicit sse: SmartStringEvaluator): Call = {

    val sectionTitle4Ga = SectionTitle4Ga.sectionTitle4GaFactory(
      formModelOptics.formModelVisibilityOptics.formModel(sectionNumber),
      sectionNumber
    )

    uk.gov.hmrc.gform.gform.routes.FormController.form(
      cache.formTemplate._id,
      maybeAccessCode,
      sectionNumber,
      sectionTitle4Ga,
      SuppressErrors.Yes,
      fastForward
    )
  }

  private def renderChooseAddressPage(
    formComponentId: FormComponentId,
    form: play.api.data.Form[String],
    addressRecords: NonEmptyList[PostcodeLookupRetrieve.AddressRecord],
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    sectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode],
    addressLookupResult: AddressLookupResult,
    title: String,
    fastForward: List[FastForward]
  )(implicit request: Request[AnyContent], l: LangADT, sse: SmartStringEvaluator): Html = {
    val addressSelectionPage = new AddressSelectionPage(
      addressRecords,
      form,
      cache.form.thirdPartyData.addressSelectionFor(formComponentId)
    )

    val backHref = goToSectionNumberLink(cache, formModelOptics, sectionNumber, maybeAccessCode, fastForward)

    val formModel = formModelOptics.formModelRenderPageOptics.formModel
    val caption = formModel.pageModelLookup.get(sectionNumber).flatMap(_.caption).map(_.value())

    addresslookup.choose_address(
      title,
      caption,
      cache.formTemplate,
      frontendAppConfig,
      addressSelectionPage,
      routes.AddressLookupController
        .submitAddress(cache.formTemplate._id, maybeAccessCode, formComponentId, sectionNumber, fastForward),
      routes.AddressLookupController
        .enterAddress(
          cache.formTemplate._id,
          maybeAccessCode,
          formComponentId,
          sectionNumber,
          SuppressErrors.Yes,
          fastForward
        ),
      backHref,
      addressLookupResult,
      maybeAccessCode
    )
  }

  private def titleForChooseAddressPage(
    formModel: FormModel[DataExpanded],
    addressLookupResult: AddressLookupResult,
    formComponentId: FormComponentId
  )(implicit sse: SmartStringEvaluator, messages: Messages) = {
    val maybeChooseAddressLabel = formModel.allFormComponents.find(_.id === formComponentId).flatMap {
      case IsPostcodeLookup(PostcodeLookup(chooseAddressLabel, _, _)) => chooseAddressLabel.map(_.value())
      case _                                                          => None
    }
    if (addressLookupResult.response.filterDisabled) {
      Messages("postcodeLookup.choose.address.all", addressLookupResult.request.postcode)
    } else maybeChooseAddressLabel.getOrElse(Messages("postcodeLookup.choose.address"))
  }

  def submitAddress(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    fastForward: List[FastForward]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        addressIdForm
          .bindFromRequest()
          .fold(
            errorForm =>
              cache.form.thirdPartyData
                .addressesFor(formComponentId)
                .fold(
                  throw new NotFoundException(s"No addresses found for FormComponentId: ${formComponentId.value}.")
                ) { case (addressRecords, addressLookupResult: AddressLookupResult) =>
                  val formModel = formModelOptics.formModelRenderPageOptics.formModel
                  val title = titleForChooseAddressPage(formModel, addressLookupResult, formComponentId)
                  BadRequest(
                    renderChooseAddressPage(
                      formComponentId,
                      errorForm,
                      addressRecords,
                      cache,
                      formModelOptics,
                      sectionNumber,
                      maybeAccessCode,
                      addressLookupResult,
                      title,
                      fastForward
                    )
                  )
                }
                .pure[Future],
            addressId =>
              addressLookupService
                .saveAddress(cache.form, maybeAccessCode, formComponentId, addressId)
                .as(
                  Redirect(
                    routes.AddressLookupController
                      .confirmAddress(
                        formTemplateId,
                        maybeAccessCode,
                        formComponentId,
                        sectionNumber,
                        fastForward
                      )
                  )
                )
          )

    }

  def confirmAddress(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    fastForward: List[FastForward]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        cache.form.thirdPartyData.addressLines(formComponentId) match {
          case None => BadRequest("TRY AGAIN").pure[Future]
          case Some(addressLines) =>
            val backHref =
              if (cache.form.thirdPartyData.enteredAddressFor(formComponentId).isDefined) {
                routes.AddressLookupController
                  .enterAddress(
                    formTemplateId,
                    maybeAccessCode,
                    formComponentId,
                    sectionNumber,
                    SuppressErrors.Yes,
                    fastForward
                  )
              } else {
                val isSingleAddress = cache.form.thirdPartyData.addressesFor(formComponentId).fold(false) {
                  case (addressRecords, _) => addressRecords.size === 1
                }

                if (isSingleAddress) {
                  goToSectionNumberLink(cache, formModelOptics, sectionNumber, maybeAccessCode, fastForward)
                } else {
                  routes.AddressLookupController
                    .chooseAddress(formTemplateId, maybeAccessCode, formComponentId, sectionNumber, fastForward)
                }
              }

            val address =
              addressLines
                .map(addresslookup.address_line(_))
                .intercalate(br())

            val confirmAddressAndContinue = routes.AddressLookupController
              .confirmAddressAndContinue(
                formTemplateId,
                maybeAccessCode,
                formComponentId,
                sectionNumber,
                fastForward
              )
            val enterAddressHref = routes.AddressLookupController
              .enterAddress(
                cache.formTemplate._id,
                maybeAccessCode,
                formComponentId,
                sectionNumber,
                SuppressErrors.Yes,
                fastForward
              )

            val formModel = formModelOptics.formModelRenderPageOptics.formModel
            val caption = formModel.pageModelLookup.get(sectionNumber).flatMap(_.caption).map(_.value())
            val maybeConfirmAddressLabel = formModel.allFormComponents.find(_.id === formComponentId).flatMap {
              case IsPostcodeLookup(PostcodeLookup(_, confirmAddressLabel, _)) => confirmAddressLabel.map(_.value())
              case _                                                           => None
            }
            val title = maybeConfirmAddressLabel.getOrElse(Messages("postcodeLookup.review.and.confirm"))
            Ok(
              addresslookup
                .review_and_confirm_address(
                  title,
                  caption,
                  cache.formTemplate,
                  frontendAppConfig,
                  address,
                  confirmAddressAndContinue,
                  enterAddressHref,
                  backHref,
                  maybeAccessCode
                )
            )
              .pure[Future]
        }
    }

  private val tryDifferentForm: play.api.data.Form[String] = play.api.data.Form(
    play.api.data.Forms.single(
      "choose" -> play.api.data.Forms.nonEmptyText
    )
  )

  def tryDifferentAddress(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    enteredPostcode: String,
    fastForward: List[FastForward]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        tryDifferentForm
          .bindFromRequest()
          .fold(
            _ => {
              val pageError = Errors(
                new components.GovukErrorSummary()(
                  ErrorSummary(
                    errorList = List(
                      ErrorLink(
                        href = Some("#choose"),
                        content = content.Text(request.messages.messages("generic.error.selectOption"))
                      )
                    ),
                    title = content.Text(request.messages.messages("error.summary.heading"))
                  )
                )
              )
              val fieldsError = Map(
                "choose" -> ErrorMessage.errorMessageWithDefaultStringsTranslated(
                  content = content.Text(request.messages.messages("generic.error.selectOption"))
                )
              )
              val renderComeBackLater =
                cache.retrievals.renderSaveAndComeBackLater && !cache.formTemplate.draftRetrievalMethod.isNotPermitted
              Ok(
                addresslookup.no_address_found(
                  cache.formTemplate,
                  frontendAppConfig,
                  enteredPostcode,
                  routes.AddressLookupController
                    .tryDifferentAddress(
                      cache.formTemplate._id,
                      maybeAccessCode,
                      formComponentId,
                      sectionNumber,
                      enteredPostcode,
                      fastForward
                    ),
                  goToSectionNumberLink(cache, formModelOptics, sectionNumber, maybeAccessCode, fastForward),
                  renderComeBackLater,
                  maybeAccessCode,
                  sectionNumber,
                  pageError,
                  fieldsError
                )
              ).pure[Future]
            },
            {
              case "Try" =>
                addressLookupService
                  .cleanAddress(cache.form, maybeAccessCode, formComponentId)
                  .as(
                    Redirect(goToSectionNumberLink(cache, formModelOptics, sectionNumber, maybeAccessCode, fastForward))
                  )
              case "Enter" =>
                Redirect(
                  routes.AddressLookupController
                    .enterAddress(
                      cache.formTemplate._id,
                      maybeAccessCode,
                      formComponentId,
                      sectionNumber,
                      SuppressErrors.Yes,
                      fastForward
                    )
                ).pure[Future]

            }
          )
    }

  def confirmAddressAndContinue(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    fastForward: List[FastForward]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => l => cache => sse => formModelOptics =>
        def resolveRedirect: Call = {
          val ff =
            routes.AddressLookupController.fastForwardAfterConfirmation(
              formTemplateId,
              maybeAccessCode,
              Some(
                sectionNumber
              ),
              fastForward
            )
          val bracket: Bracket[Visibility] =
            formModelOptics.formModelVisibilityOptics.formModel.brackets.withSectionNumber(sectionNumber)

          bracket.fold { nonRepeatingPage =>
            ff
          } { repeatingPage =>
            ff
          } { addToList =>
            val iteration: Bracket.AddToListIteration[Visibility] = addToList.iterationForSectionNumber(sectionNumber)
            routes.AddressLookupController.fastForwardAfterConfirmation(
              formTemplateId,
              maybeAccessCode,
              iteration.checkYourAnswers
                .map(_.sectionNumber)
                .orElse(Some(sectionNumber)),
              fastForward
            )
          }
        }

        val updatedForm = addressLookupService
          .populatePostcodeIfEmpty(
            cache.form,
            maybeAccessCode,
            formComponentId,
            formModelOptics.formModelVisibilityOptics,
            sectionNumber
          )

        addressLookupService
          .flagAddressAsConfirmed(updatedForm, maybeAccessCode, formComponentId)
          .as(Redirect(resolveRedirect))
    }

  def fastForwardAfterConfirmation(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    maybeSectionNumber: Option[SectionNumber],
    fastForward: List[FastForward]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => sse => formModelOptics =>
        maybeSectionNumber.fold(
          fastForwardService
            .redirectFastForward[SectionSelectorType.Normal](
              cache,
              maybeAccessCode,
              formModelOptics,
              maybeSectionNumber,
              fastForward
            ) // TODO JoVl Revisit maybeCoordinates param
        ) { sn =>
          fastForwardService
            .redirectFastForward[SectionSelectorType.Normal](
              cache,
              maybeAccessCode,
              formModelOptics,
              maybeSectionNumber,
              fastForward
            )
        }

    }

  def enterAddress(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    se: SuppressErrors,
    fastForward: List[FastForward]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => realFormModelOptics =>
        mkSyntheticFormModelOptics(formComponentId, cache, maybeAccessCode, realFormModelOptics) {
          syntheticFormComponent => syntheticCache => formModelOptics =>
            val cacheData = syntheticCache.toCacheData

            val envelopeWithMapping: EnvelopeWithMapping = EnvelopeWithMapping(Envelope.empty, syntheticCache.form)

            formControllerRequestHandler
              .handleSuppressErrors(
                formModelOptics,
                formModelOptics.formModelVisibilityOptics.formModel.availableSectionNumbers,
                cacheData,
                envelopeWithMapping,
                validationService.validatePageModel,
                se
              )
              .map { formHandlerResult =>
                val formFieldValidationResult: FormFieldValidationResult =
                  formHandlerResult.validationResult(syntheticFormComponent)

                val enterAddressPage = new EnterAddressPage(syntheticFormComponent, formFieldValidationResult)
                val formAction =
                  routes.AddressLookupController
                    .enterAddressSubmit(
                      formTemplateId,
                      maybeAccessCode,
                      formComponentId,
                      sectionNumber,
                      fastForward
                    )

                val formModel = realFormModelOptics.formModelRenderPageOptics.formModel
                val maybeEnterAddressLabel = formModel.allFormComponents.find(_.id === formComponentId).flatMap {
                  case IsPostcodeLookup(PostcodeLookup(_, _, enterAddressLabel)) => enterAddressLabel.map(_.value())
                  case _                                                         => None
                }
                val title = maybeEnterAddressLabel.getOrElse(Messages("postcodeLookup.enter.address"))
                val caption = formModel.pageModelLookup.get(sectionNumber).flatMap(_.caption).map(_.value())

                val backHref =
                  if (cache.form.thirdPartyData.addressRecordFor(formComponentId).isDefined) {
                    routes.AddressLookupController
                      .chooseAddress(
                        formTemplateId,
                        maybeAccessCode,
                        formComponentId,
                        sectionNumber,
                        fastForward
                      )
                  } else {
                    goToSectionNumberLink(cache, realFormModelOptics, sectionNumber, maybeAccessCode, fastForward)
                  }
                Ok(
                  addresslookup
                    .enter_address(
                      title,
                      caption,
                      cache.formTemplate,
                      frontendAppConfig,
                      enterAddressPage,
                      formAction,
                      backHref,
                      fastForward,
                      maybeAccessCode
                    )
                )
              }
        }
    }

  def enterAddressSubmit(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    fastForward: List[FastForward]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => realFormModelOptics =>
        mkSyntheticFormModelOptics(formComponentId, cache, maybeAccessCode, realFormModelOptics) {
          syntheticFormComponent => syntheticCache => formModelOptics =>
            processResponseDataFromBody(request, formModelOptics.formModelRenderPageOptics, None) {
              requestRelatedData => variadicFormData => enteredVariadicFormData =>
                val browserFormModelOpticsF = FormModelOptics
                  .mkFormModelOptics[DataOrigin.Browser, Future, SectionSelectorType.Normal](
                    variadicFormData,
                    syntheticCache,
                    recalculation
                  )

                val envelopeWithMapping: EnvelopeWithMapping = EnvelopeWithMapping(Envelope.empty, syntheticCache.form)

                val cacheData = syntheticCache.toCacheData

                browserFormModelOpticsF.flatMap { browserFormModelOptics =>
                  addressLookupService.saveEnteredAddress(
                    cache.form,
                    maybeAccessCode,
                    formComponentId,
                    variadicFormData.toFormData
                  ) >>
                    formControllerRequestHandler
                      .handleFormValidation(
                        browserFormModelOptics,
                        browserFormModelOptics.formModelVisibilityOptics.formModel.availableSectionNumbers.headOption
                          .getOrElse(cache.formTemplate.sectionNumberZero),
                        cacheData,
                        envelopeWithMapping,
                        validationService.validatePageModel,
                        enteredVariadicFormData
                      )
                      .map { formValidationOutcome =>
                        if (formValidationOutcome.isValid) {
                          Redirect(
                            routes.AddressLookupController.confirmAddressAndContinue(
                              formTemplateId,
                              maybeAccessCode,
                              formComponentId,
                              sectionNumber,
                              fastForward
                            )
                          )
                        } else {
                          Redirect(
                            routes.AddressLookupController
                              .enterAddress(
                                formTemplateId,
                                maybeAccessCode,
                                formComponentId,
                                sectionNumber,
                                SuppressErrors.No,
                                fastForward
                              )
                          )
                        }
                      }
                }
            }
        }
    }

  private def mkSyntheticFormComponent(
    formComponentId: FormComponentId,
    formComponent: Option[FormComponent]
  ): FormComponent =
    FormComponent(
      id = formComponentId,
      `type` = Address(false, List.empty[Address.Configurable.Mandatory], false, None),
      label = SmartString(LocalisedString(Map.empty), List.empty[Expr]),
      isPageHeading = false,
      helpText = None,
      shortName = None,
      includeIf = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = false,
      errorMessage = None,
      errorShortName = formComponent.flatMap(_.errorShortName),
      errorShortNameStart = formComponent.flatMap(_.errorShortNameStart),
      validators = formComponent.map(_.validators).getOrElse(Nil)
    )

  private def mkSyntheticCache(
    cache: AuthCacheWithForm,
    syntheticFormComponent: FormComponent
  ): AuthCacheWithForm = {
    val title = SmartString(LocalisedString(Map.empty), List.empty[Expr])
    val fields = List(syntheticFormComponent)
    val page: Page[Basic] = Page(
      title = title,
      id = None,
      noPIITitle = None,
      description = None,
      shortName = None,
      caption = None,
      includeIf = None,
      fields = fields,
      continueLabel = None,
      continueIf = None,
      instruction = None,
      presentationHint = None,
      dataRetrieve = None,
      confirmation = None,
      redirects = None,
      hideSaveAndComeBackButton = None,
      removeItemIf = None,
      displayWidth = None
    )
    val enterAddressSection = Section.NonRepeatingPage(page)
    val syntheticFormTemplate = cache.formTemplate.copy(formKind =
      cache.formTemplate.formKind
        .fold[FormKind](classic => classic.copy(sections = List(enterAddressSection)))(taskList =>
          taskList.copy(sections =
            taskList.sections.map(taskSection =>
              taskSection.copy(tasks = taskSection.tasks.map(_.copy(sections = NonEmptyList.of(enterAddressSection))))
            )
          )
        )
    )

    val formData =
      cache.form.thirdPartyData
        .enteredAddressDataForWithFallback(syntheticFormComponent.id)
        .getOrElse(FormData(List.empty))

    cache
      .copy(formTemplateContext = FormTemplateContext.basicContext(syntheticFormTemplate, None))
      .copy(form = cache.form.copy(formData = formData))
  }

  private def mkSyntheticFormModelOptics(
    formComponentId: FormComponentId,
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(f: FormComponent => AuthCacheWithForm => FormModelOptics[DataOrigin.Mongo] => Future[Result])(implicit
    messages: Messages,
    l: LangADT,
    hc: HeaderCarrier
  ): Future[Result] = {
    val maybeFormComponent = formModelOptics.formModelVisibilityOptics.formModel.fcLookup
      .get(formComponentId)
    val syntheticFormComponent = mkSyntheticFormComponent(formComponentId, maybeFormComponent)

    val syntheticCache = mkSyntheticCache(cache, syntheticFormComponent)

    val formModelBuilder = new FormModelBuilder[Throwable, Future](
      syntheticCache.retrievals,
      syntheticCache.formTemplate,
      syntheticCache.form.thirdPartyData,
      syntheticCache.form.envelopeId,
      maybeAccessCode,
      recalculation,
      FormComponentIdToFileIdMapping.empty,
      lookupOptions
    )

    val data = syntheticCache.variadicFormData[SectionSelectorType.Normal]

    val formModelVisibilityOpticsF: Future[FormModelVisibilityOptics[DataOrigin.Mongo]] =
      formModelBuilder.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)

    formModelVisibilityOpticsF
      .map { formModelVisibilityOptics =>
        formModelBuilder
          .renderPageModel[DataOrigin.Mongo, SectionSelectorType.Normal](formModelVisibilityOptics, None)
      }
      .flatMap(f(syntheticFormComponent)(syntheticCache))
  }
}
