;(function (global) {
  'use strict'

  var $ = global.jQuery;
  var GOVUK = global.GOVUK || {};

  function GformRepeatingGroups () {
    var self = this;

    function init () {
      // check we are on a page with repeating groups
      var isRepeatingPage = $('.section-repeating').length;
      if (!isRepeatingPage) return;

      var fieldset = $(window.location.hash).parent()
      if (fieldset.context) $('html, body').animate({ scrollTop: $(fieldset).offset().top}, 1000);
    }

    self.GformRepeatingGroups = function () {
      init()
    }

  }

  GformRepeatingGroups.prototype.init = function () {
    this.GformRepeatingGroups()
  }

  GOVUK.GformRepeatingGroups = GformRepeatingGroups;
  global.GOVUK = GOVUK
})(window);

