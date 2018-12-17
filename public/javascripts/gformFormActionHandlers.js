;(function (global) {
    'use strict'
  
    var $ = global.jQuery
    var GOVUK = global.GOVUK || {}
  
    function GformFormActionHandlers () {
      var self = this

      function setAction (e, action, submit) {
        action = action || $(e.target).val();
        $('#gform-action').val(action);
        if (submit) {
          e.preventDefault();
          $('#gf-form').submit();
        }
      }
      
      function handleFormSubmit(action, submit) {
        return function (e) {
          setAction(e, action, submit);
        };
      }
  
      // Set up event handlers
      function init () {
        $('#content')
          .on('click', '[type="submit"]', setAction)
          .on('click', '.removeRepeatingSection, #addRepeatingGroup', handleFormSubmit(null, true))
          .on('click', '#backButton', handleFormSubmit('Back', true))
          .on('click', '#BackToSummary', handleFormSubmit('BackToSummary', true))
          .on('click', '#saveComeBackLater', handleFormSubmit('Save', true))
          .on('click', '#saveComBackLaterExit', handleFormSubmit('Exit', true));
      }
      
      self.GformFormActionHandlers = function () {
        init()
      }
  
    }
  
    GformFormActionHandlers.prototype.init = function () {
      this.GformFormActionHandlers()
    }
  
    GOVUK.GformFormActionHandlers = GformFormActionHandlers
    global.GOVUK = GOVUK
  })(window)