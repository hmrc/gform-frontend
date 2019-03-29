;(function (global) {
  'use strict'

  var $ = global.jQuery;
  var GOVUK = global.GOVUK || {};

  function GformSummaryLayout () {
    var self = this;

    function adjustSummarySection($textareaCells) {
      $textareaCells.each(function (i, textareaCell) {
        var $cell = $(textareaCell);
        var content = $cell
          .text()
          .trim()
          .split(/\r?\n/g);
        $cell
          .html(content.join('<br>'))
          .closest('dl')
          .removeClass('cya-questions-long')
          .addClass('cya-questions-short')
      });
    }

    function init () {
      var $textareaCells = $('dd.cya-textarea');
      if($textareaCells.length) {
        adjustSummarySection($textareaCells)
      }
    }

    self.GformSummaryLayout = function () {
      init()
    }

  }

  GformSummaryLayout.prototype.init = function () {
    this.GformSummaryLayout()
  }

  GOVUK.GformSummaryLayout = GformSummaryLayout;
  global.GOVUK = GOVUK
})(window);

