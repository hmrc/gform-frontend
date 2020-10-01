;(function (global) {
    'use strict'

    var $ = global.jQuery
    var GOVUK = global.GOVUK || {}

    function GformFormActionHandlers () {
      var self = this;

      var submitButton = $('button.govuk-button')

      function disableSubmitButton() {
        submitButton.attr('disabled', true)
      }

      function findAction ($el) {
        return $el.is("button")
          ? $el.val()
          : $el.is("a")
          ? $el.attr("href")
          : $el.is("span") &&
            $el.attr("aria-hidden") == "true" &&
            $el.parent().is("a")
          ? $el.parent().attr("href")
          : "";
      }

      function setAction (e, action, submit) {
        action = action || findAction($(e.target));
        $('#gform-action').val(action);
        if (submit) {
          e.preventDefault();
          $('#gf-form').submit();
          disableSubmitButton();
        }
      }

      function handleFormSubmit(action, submit) {
        return function (e) {
          setAction(e, action, submit);
        };
      }

      // Set up event handlers
      function init () {
        // Prevent form submissions while submit button is disabled (covers form submission by hitting Enter)
        $('form').submit(function(e) {
          if ($(this).find('input[type=submit], button[type=submit]').prop('disabled')) {
            return false
          }
        })

	$("#main-content")
          .parent()
          .on('click', 'button.govuk-button', handleFormSubmit(null, true))
          .on('click', '.removeRepeatingSection, #addRepeatingGroup', handleFormSubmit(null, true))
	  .on("click", "#editAddToList", handleFormSubmit(null, true))
	  .on('click', '#removeAddToList', handleFormSubmit(null, true))
          .on('click', '#backButton', handleFormSubmit('Back', true))
          .on('click', '#saveComeBackLater', handleFormSubmit('Save', true))
          .on('click', '#saveComeBackLaterExit', handleFormSubmit('Exit', true));

       // update any character counters with ids and aria labels
        $('.char-counter-text').each(function (i, hint) {
          var id = 'character-info-' + i;
          var $hint = $(hint);
          $hint.attr('id', id).attr('aria-live', 'polite');
          var $textarea = $hint.siblings('textarea');
          var existingAttr = $textarea.attr('aria-describedby')
            if(existingAttr){
                $textarea.attr('aria-describedby', existingAttr + " " + id);
            } else {
                $textarea.attr('aria-describedby', id);
            }

          })
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
