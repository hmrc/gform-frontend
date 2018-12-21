;(function(){
  var showHideContent = new GOVUK.ShowHideContent();
  showHideContent.init();

  GOVUK.details.init()

  var gformFileUpload = new GOVUK.GformFileUpload();
  gformFileUpload.init();

  var formActionHandlers = new GOVUK.GformFormActionHandlers();
  formActionHandlers.init();

  var errorSummary = new GOVUK.GformErrorSummary();
  errorSummary.init();
  
  var toggleAddressLabels = new GOVUK.GformToggleAddressLabels();
  toggleAddressLabels.init();

  var gformGAEvents = new GOVUK.GformGAEvents();
  gformGAEvents.init();

  GOVUK.gformSessionTimeout({})
})();
