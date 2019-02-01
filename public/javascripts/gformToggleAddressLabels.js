;(function (global) {
    'use strict'
  
    var $ = global.jQuery
    var GOVUK = global.GOVUK || {}
  
    function GformToggleAddressLabels () {
      var self = this

      function showLabel($label) {
        $label.removeClass('js-hidden').attr('aria-hidden', false)
      }
    
      function hideLabel($label) {
        $label.find('input').val('')
        $('#' + $($label).parent().attr('for')).val('')
        $label.addClass('js-hidden').attr('aria-hidden', true)
      }
    
      function toggleAddressLabels(e) {

        var addressType = $(e.target).attr('aria-controls');
        $("div[id*='address-']").each(function (i, whichDiv) {
          $(whichDiv).attr('id') === addressType ? showLabel($(whichDiv)) : hideLabel($(whichDiv))
        })

        var choice = $(e.target).attr('data-address-choice');
        $('span[data-address-label]').each(function (i, label) {
          $(label).attr('data-address-label') === choice ? showLabel($(label)) : hideLabel($(label))
        })
      }

      // Set up event handlers
      function init () {
        $('input[data-address-choice]').on('click', toggleAddressLabels);
      }
      
      self.GformToggleAddressLabels = function () {
        init()
      }
  
    }
  
    GformToggleAddressLabels.prototype.init = function () {
      this.GformToggleAddressLabels()
    }
  
    GOVUK.GformToggleAddressLabels = GformToggleAddressLabels
    global.GOVUK = GOVUK
  })(window)

  