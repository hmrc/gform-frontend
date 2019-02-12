;(function(window){
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

  if (window.gform && window.gform.config && window.gform.config.timeoutEnabled) {
    GOVUK.gformSessionTimeout({
      timeout: window.gform.config.timeout,
      countdown: window.gform.config.countdown
    })
  }

})(window);
