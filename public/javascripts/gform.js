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

  var gformSummaryLayout = new GOVUK.GformSummaryLayout();
  gformSummaryLayout.init();

  var gformRepeatingGroups = new GOVUK.GformRepeatingGroups();
  gformRepeatingGroups.init();

  var gformAutoComplete = new GOVUK.GformAutoComplete();
  gformAutoComplete.init();

  if (window.gform && window.gform.config && window.gform.config.timeoutEnabled) {
    GOVUK.gformSessionTimeout({
      timeout: 30,
      countdown: 15,
      keep_alive_url: window.gform.config.keep_alive_url,
      logout_url: window.gform.config.logout_url
    })
  }

})(window);
