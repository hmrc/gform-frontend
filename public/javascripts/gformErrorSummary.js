;(function (global) {
    'use strict'
  
    var $ = global.jQuery
    var GOVUK = global.GOVUK || {}
  
    function GformErrorSummary () {
      var self = this

      function setUpErrorSummary($el) {
        var links = [];
        // remove multiple links to same context
        $el.find('a').each(function (i, link) {
          var $link = $(link);
          var context = $link.attr('data-context');
          if (links.indexOf(context) === -1) {
            links.push(context);
            $link.removeClass('js-hidden').on('click', function () {
              var focuses = $(this).attr('data-focuses');
              $('[name="' + focuses + '"]').first().trigger('focus')
            });
          } else {
            $link.parents('li').remove()
          }
        });
        // focus on the error summary
        $el.trigger('focus');
        // remove subsequent inline error messages for same field
        $('.error-message + .error-message').remove()
      }
  
      // Set up event handlers
      function init () {
        setUpErrorSummary($('.error-summary'));
      }
      
      self.GformErrorSummary = function () {
        init()
      }
    }
  
    GformErrorSummary.prototype.init = function () {
      this.GformErrorSummary()
    }
  
    GOVUK.GformErrorSummary = GformErrorSummary
    global.GOVUK = GOVUK
  })(window)

  
    