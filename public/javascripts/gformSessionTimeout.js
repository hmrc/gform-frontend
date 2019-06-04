;(function (global, document) {
  'use strict';
  var $ = global.jQuery;
  var GOVUK = global.GOVUK || {};
  var lang = global.gform && global.gform.lang || "en";
  var strings = {
    title: {
      en: "You’re about to be signed out",
      cy: "Rydych chi ar fin cael eich llofnodi"
    },
    time: {
      en: "minutes",
      cy: "munudau"
    },
    time_singular: {
      en: "minute",
      cy: "munud"
    },
    seconds: {
      en: "seconds",
      cy: "eiliadau"
    },
    message: {
      en: "For security reasons, you will be signed out of this service in",
      cy: "Am resymau diogelwch, fe’ch llofnodir o’r gwasanaeth hwn"
    },
    keep_alive_button_text: {
      en: "Stay signed in",
      cy: "Arhoswch i mewn"
    },
    sign_out_button_text: {
      en: "Sign out",
      cy: "Cofrestrwch"
    }
  };

  GOVUK.gformSessionTimeout = function(options) {

    var settings = {
      timeout: 900,
      countdown: 120,
      time: strings.time[lang],
      timeSingular: strings.time_singular[lang],
      title: strings.title[lang],
      message: strings.message[lang],
      keep_alive_url: '/submissions/keep-alive',
      logout_url: '/loggedout',
      restart_on_yes: true,
      dialog_width: 340,
      close_on_escape: true,
      keep_alive_button_text: strings.keep_alive_button_text[lang],
      sign_out_button_text: strings.sign_out_button_text[lang]
    };

    $.extend(settings, options);

    var timeoutInterval, startTime, currentMin, activeElement;
    var dialogOpen = false;
    var $pageElements = $('#skiplink-container, body>header, #global-cookie-message, body>main, body>footer');
    var $html = $('html');
    var $document = $(document);

    function getDateNow() {
        return Date.now() || +new Date()
    }

    function secondsToTime (secs) {
      var hours = Math.floor(secs / (60 * 60));
      var divisorForMinutes = secs % (60 * 60);
      var minutes = Math.floor(divisorForMinutes / 60);
      var divisorForSeconds = divisorForMinutes % 60;
      var seconds = Math.ceil(divisorForSeconds);
      return {
        'h': hours,
        'm': minutes,
        's': seconds
      };
    }

    function expired() {
      return getDateNow() >= settings.signout_time
    }

    function withinCountdown() {
      return !expired() && getDateNow() >=  settings.signout_time - (settings.countdown * 1000)
    }

    function init() {
      setupDialogTimer()
    }

    function setupDialogTimer() {
      dialogOpen = false;
      settings.signout_time = getDateNow() + settings.timeout * 1000;
      timeoutInterval = global.setInterval(checkTimeElapsed, 1000)
    }

    function checkTimeElapsed() {
      if (expired()) {
        signOut()
      } else if (withinCountdown()) {
        global.clearInterval(timeoutInterval);
        setupDialog()
      }
    }

    var escPress = function (event) {
      if (event.keyCode === 27) {
        keepAlive();
      }
    };

    var keepAliveAndCloseDialog = function () {
      if (dialogOpen) {
        keepAlive();
      }
    };

    var handleTouch = function (e) {
      var touches = e.originalEvent.touches || e.originalEvent.changedTouches;
      if ($('#timeout-dialog').length) {
        if (touches.length === 1) {
          e.preventDefault()
        }
      }
    };

    function setupDialog() {
      destroyDialog();
      activeElement = document.activeElement;
      dialogOpen = true;
      startTime = Math.round(getDateNow() / 1000);
      currentMin = Math.ceil(settings.timeout / 60);
      $html.addClass('noScroll');
      var time = secondsToTime(settings.countdown);
      if (time.m === 1) {
        settings.time = ' ' + settings.timeSingular
      }
      $('<div id="timeout-dialog" class="timeout-dialog" role="dialog" aria-labelledby="timeout-message" tabindex=-1 aria-live="polite">' +
        '<h1 class="heading-medium push--top">' + settings.title + '</h1>' +
        '<p id="timeout-message" role="text">' + settings.message + ' <span id="timeout-countdown" class="countdown">' + time.m + ' ' + settings.time + '</span>' + '.</p>' +
        '<button id="timeout-keep-alive-btn" class="button">' + settings.keep_alive_button_text + '</button>' +
        '<button id="timeout-sign-out-btn" class="button button--link">' + settings.sign_out_button_text + '</button>' +
        '</div>' +
        '<div id="timeout-overlay" class="timeout-overlay"></div>')
        .appendTo('body');

      // AL: disable the non-dialog page to prevent confusion for VoiceOver users
      $pageElements.attr('aria-hidden', 'true');
      var modalFocus = document.getElementById('timeout-dialog');
      modalFocus.focus();
      addEvents();

      startCountdown(settings.countdown);

    }

    function destroyDialog() {
      var $dialogue = $('#timeout-dialog');
      if (!$dialogue.length) {
        return false;
      }
      dialogOpen = false;
      removeEvents();
      $('.timeout-overlay').remove();
      $dialogue.remove();
      $html.removeClass('noScroll');
      $pageElements.removeAttr('aria-hidden');
      activeElement.focus();
    }

    function updateUI(counter) {
      var $countdownEl = $('#timeout-countdown');
      if (counter < 60) {
        $('.timeout-dialog').removeAttr('aria-live');
        $countdownEl.html(counter + ' ' + strings.seconds[lang])
      } else {
        var newCounter = Math.ceil(counter / 60);
        var minutesMessage = ' ' + settings.time;
        if (newCounter === 1) {
          minutesMessage = ' ' + settings.timeSingular
        }
        if (newCounter < currentMin) {
          currentMin = newCounter;
          $countdownEl.html(newCounter + minutesMessage)
        }
      }
    }

    function setFocusOnActiveDialog(event) {
      var modalFocus = document.getElementById('timeout-dialog');
      if (modalFocus && dialogOpen) {
        if (!modalFocus.contains(event.target)) {
          event.stopPropagation();
          modalFocus.focus()
        }
      }
    }

    function addEvents() {
      $document
        .on('touchmove', handleTouch)
        .on('keydown', escPress)
        .on('click', '#timeout-keep-alive-btn', keepAliveAndCloseDialog)
        .on('click', '#timeout-sign-out-btn', signOut)
        .on('focus', 'a, input, textarea, button, [tabindex!="-1"]', setFocusOnActiveDialog);
    }

    function removeEvents() {
      $document
        .off('touchmove', handleTouch)
        .off('keydown', escPress)
        .off('click', '#timeout-keep-alive-btn', keepAliveAndCloseDialog)
        .off('click', '#timeout-sign-out-btn', signOut)
        .off('focus', 'a, input, textarea, button, [tabindex!="-1"]', setFocusOnActiveDialog);
    }

    function startCountdown(counter) {
      global.countdown = global.setInterval(function () {
        if (expired()) {
          signOut()
        }
        counter -= 1;
        updateUI(counter);
      }, 1000)
    }

    function keepAlive() {
      destroyDialog();
      global.clearInterval(global.countdown);
      $.get(settings.keep_alive_url, function () {
        if (settings.restart_on_yes) {
          setupDialogTimer()
        } else {
          signOut()
        }
      })
    }

    function signOut() {
      global.location = settings.logout_url
    }

    init()
  };
})(window, document);
