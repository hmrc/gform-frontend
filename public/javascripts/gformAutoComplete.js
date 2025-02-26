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
      var lang = $container.attr('data-language');

      var configurationOptions = {
              selectElement: $container[0],
              id: id,
              name: id,
              source: generateSourceFn(lookup, formTemplateId, formComponentId, maybeAccessCode),
              showNoOptionsFound: true,
              autoselect: false,
              defaultValue: value,
              showAllValues: showAll === "Enabled",
              dropdownArrow: function(config) {
                 return ''
               },
              onConfirm: function(){}
      };

     if (lang === 'cy') {
       //copied from https://github.com/hmrc/hmrc-frontend/blob/main/src/components/accessible-autocomplete/accessible-autocomplete.js
       configurationOptions.tAssistiveHint = function(){ return'Pan fydd canlyniadau awtogwblhau ar gael, defnyddiwch y saethau i fyny ac i lawr i’w hadolygu a phwyswch y fysell ’enter’ i’w dewis.'
           + ' Gall defnyddwyr dyfeisiau cyffwrdd, archwilio drwy gyffwrdd â’r sgrin neu drwy sweipio.' };
       configurationOptions.tStatusQueryTooShort = function(minQueryLength){ return 'Ysgrifennwch '+ minQueryLength + ' neu fwy o gymeriadau am ganlyniadau' };
       configurationOptions.tNoResults = function(){ return 'Dim canlyniadau wedi’u darganfod' };
       configurationOptions.tStatusNoResults = function(){ return 'Dim canlyniadau chwilio' };
       configurationOptions.tStatusSelectedOption = function(selectedOption, length, index) { return 'Mae ' + selectedOption + (index + 1) + ' o '+ length +' wedi’i amlygu' };
       configurationOptions.tStatusResults = function(length, contentSelectedOption) {
         var resultOrResults = (length === 1) ? 'canlyniad' : 'o ganlyniadau';
         return length + ' ' + resultOrResults +' ar gael. '+ contentSelectedOption;
       };
     }
      // this is the method provided by accessible-autocomplete.min.js
     window.accessibleAutocomplete.enhanceSelectElement(configurationOptions);

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
