;(function(window){

  var gformFileUpload = new GOVUK.GformFileUpload();
  gformFileUpload.init();

  var formActionHandlers = new GOVUK.GformFormActionHandlers();
  formActionHandlers.init();

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

})(window);
