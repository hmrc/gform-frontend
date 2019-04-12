;(function (global) {
  'use strict';

  var $ = global.jQuery;
  var GOVUK = global.GOVUK || {};

  function GformAutoComplete () {
    // this url should come from appConfig, if it's missing the fallback
    // is used
    var baseLookupUrl = global.gform.baseLookupUrl || "/submissions/lookup/";

    function generateSourceFn (lookup) {
      var URL = baseLookupUrl + lookup;
      return function (query, populateResults) {
        return $.get(URL + '/' + encodeURIComponent(query))
          .then(populateResults)
      }
    }

    function setUpAutoComplete ($container) {
      var lookup = $container.attr('data-lookup');
      var id = $container.attr('data-field-id');
      var value = $container.attr('data-value');
      var showAll = $container.attr('data-show-all');
      function blockAutoFill ($input) {
        var val = $input.val();
        $input
          .attr('name', 'random' + Date.now())
          .val(val)
      }

      function resetInput ($input) {
        var val = $input.val();
        $input
          .attr('name', $input.attr('id'))
          .attr('autocomplete', 'lookup' + Date.now())
          .val(val)
      }

      // this is the method provided by accessible-autocomplete.min.js
      window.accessibleAutocomplete({
        element: $container[0],
        id: id,
        source: generateSourceFn(lookup),
        showNoOptionsFound: false,
        defaultValue: value,
        showAllValues: showAll
      });

      var checkInput = window.setInterval(function () {
        var $input = $('#' + id);
        if ($input.length) {
          window.clearInterval(checkInput);
          resetInput($input)
        }
      }, 200);

      function handleBlockAutoFill (e) {
        blockAutoFill($(e.currentTarget))
      }

      function handleResetInput (e) {
        resetInput($(e.currentTarget))
      }

      $container
        .on('focus', 'input', handleBlockAutoFill)
        .on('blur', 'input', handleResetInput)
    }

    function init () {
      var $els = $('.lookup');
      if ($els.length && typeof window.accessibleAutocomplete === 'function') {
        $els.each(function (i, el) {
          setUpAutoComplete($(el))
        })
      }
    }

    self.GformAutoComplete = function () {
      init()
    }
  }

  GformAutoComplete.prototype.init = function () {
    this.GformAutoComplete()
  };

  GOVUK.GformAutoComplete = GformAutoComplete;
  global.GOVUK = GOVUK
})(window);
