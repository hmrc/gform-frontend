;(function (global) {
    'use strict'
  
    var $ = global.jQuery
    var GOVUK = global.GOVUK || {}
  
    function GformGAEvents () {
      var self = this

      // wrapper function to ensure ga is
      // available in the environment
      function sendToGA() {
        if (typeof global.ga === 'function') {
          global.ga.apply(null, arguments)
        }
      }


      function labelText($errorLink) {
        var fieldId = $errorLink.attr('data-focuses');
        var $label = $('label[for="' + fieldId + '"]');
        if ($label.length) {
        return $label.text().trim();
        }
        var $legend = $('fieldset#' + fieldId).find('legend.form-label').first();
        if ($legend.length) {
        return $legend.text().trim()
        }

        return $('h1').text().trim();
      }

      function sendErrorToGA($errorLink) {
        // Google Analytics event reporting, using template:
        // ga('send', 'event', [eventCategory], [eventAction], [eventLabel], [eventValue], [fieldsObject])
        sendToGA('send', 'event', 'error - field', labelText($errorLink), $errorLink.text().trim())
      }

      // Set up event handlers
      function init () {
        $('.error-summary').find('a').each(function(i, link) {
          sendErrorToGA($(link));
        })
      }
      
      self.GformGAEvents = function () {
        init()
      }
    }
  
    GformGAEvents.prototype.init = function () {
      this.GformGAEvents()
    }
  
    GOVUK.GformGAEvents = GformGAEvents
    global.GOVUK = GOVUK
  })(window)

  
    