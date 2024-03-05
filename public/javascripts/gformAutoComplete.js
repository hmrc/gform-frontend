;(function (global) {
  'use strict';

  var $ = global.jQuery;
  var GOVUK = global.GOVUK || {};

  function GformAutoComplete () {
    // this url should come from appConfig, if it's missing the fallback
    // is used
    var baseLookupUrl = global.gform.baseLookupUrl || "/submissions/lookup/";

    function generateSourceFn (lookup, formTemplateId, id, maybeAccessCode) {
      var URL = baseLookupUrl + formTemplateId + "/" + id + "/" + lookup;
      return function (query, populateResults) {
        return $.get(URL + '/' + maybeAccessCode + "?query=" + encodeURIComponent(query))
          .then(populateResults)
      }
    }

    function setUpAutoComplete ($container) {
      var lookup = $container.attr('data-lookup');
      var formTemplateId = $container.attr('data-formTemplateId');
      var id = $container.attr('data-field-id');
      var formComponentId = $container.attr('data-component-id');
      var value = $container.attr('data-value');
      var showAll = $container.attr('data-show-all');
      var maybeAccessCode = $container.attr('data-accessCode');
      var displayWidth = $container.attr('data-displayWidth');

      // this is the method provided by accessible-autocomplete.min.js
      window.accessibleAutocomplete({
        element: $container[0],
        id: id,
        name: id,
        source: generateSourceFn(lookup, formTemplateId, formComponentId, maybeAccessCode),
        showNoOptionsFound: true,
        autoselect: false,
        defaultValue: value,
        showAllValues: showAll === "Enabled",
        dropdownArrow: function(config) {
           return '<svg class="' + config.className +
             '" style="top: 8px; z-index:1" viewBox="0 0 512 512" ><path d="M256,298.3L256,298.3L256,298.3l174.2-167.2c4.3-4.2,11.4-4.1,15.8,0.2l30.6,29.9c4.4,4.3,4.5,11.3,0.2,15.5L264.1,380.9  c-2.2,2.2-5.2,3.2-8.1,3c-3,0.1-5.9-0.9-8.1-3L35.2,176.7c-4.3-4.2-4.2-11.2,0.2-15.5L66,131.3c4.4-4.3,11.5-4.4,15.8-0.2L256,298.3  z"/></svg>'
         },
      });

      function getMaxWidth(displayWidth) {
        var maxWidth = null;
        switch(displayWidth) {
            case "XS": maxWidth = "9ex"; break;
            case "S": maxWidth = "10.8ex"; break;
            case "M": maxWidth = "23ex"; break;
            case "L": maxWidth = "41ex"; break;
            case "XL": maxWidth = "59ex"; break;
            case "XXL": break;
            case "DEFAULT": maxWidth = "41ex"; break;
        }
        if(maxWidth) {
           return {"max-width": maxWidth }
        } else {
           return {}
        }
      }

      $container.css(getMaxWidth(displayWidth))

      var checkInput = window.setInterval(function () {
        var $input = $('input#' + id);
        if ($input.length) {
          $input
            .attr('autocomplete', 'off');
          window.clearInterval(checkInput);
        }
      }, 200);
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
    self.GformAutoComplete()
  };

  GOVUK.GformAutoComplete = GformAutoComplete;
  global.GOVUK = GOVUK
})(window);
