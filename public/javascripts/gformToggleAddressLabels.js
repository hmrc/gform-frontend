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
        $label.addClass('js-hidden').attr('aria-hidden', true)
        $('#' + $label.parent().attr('for')).val('')
      }
    
      function toggleAddressLabels(e) {

        var choice = $(e.target).attr('data-address-choice');
        var scopeVar = $(e.target).closest('[id$="-fieldset"]')

        scopeVar.find('span[data-address-label]').each(function (i, label) {
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

  