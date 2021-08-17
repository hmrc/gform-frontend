;(function (global) {
    'use strict'

    var $ = global.jQuery
    var GOVUK = global.GOVUK || {}

    function GformFormActionHandlers () {
      var self = this;

      var submitButtons = $('button[type=submit]')

      function disableSubmitButtons() {
        submitButtons.attr('disabled', true)
      }

      function findAction ($el) {
        return $el.is("a")
          ? $el.attr("href")
          : $el.is("span") &&
            $el.attr("aria-hidden") == "true" &&
            $el.parent().is("a")
          ? $el.parent().attr("href")
          : "";
      }

      function setFormActionAndSubmit(e, action) {
          $('#gf-form').attr('action', action || findAction($(e.target)));
          e.preventDefault();
          $('#gf-form').submit();
          disableSubmitButtons();
      }

      function handleBackButton(e) {
        var $input = $(e.currentTarget);
        var dataset = $input[0].dataset;
        var formTemplateId = dataset.formTemplateId;
        var accessCode = dataset.accessCode;
        var sectionNumber = dataset.sectionNumber;
        var fastForward = dataset.fastForward
        setFormActionAndSubmit(e, "/submissions/form/" + formTemplateId + "/" + accessCode + "/" + sectionNumber + "?ff=" + fastForward + "&action=Back");
      }

      // Set up event handlers
      function init () {
        // Prevent form submissions while submit button is disabled (covers form submission by hitting Enter)
        $('form').submit(function(e) {
          if ($(this).find('button[type=submit]').prop('disabled')) {
            return false
          }
          disableSubmitButtons()
          return true
        })

	    $("#main-content")
          .parent()
          .on('click', '#backButton', handleBackButton)

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
