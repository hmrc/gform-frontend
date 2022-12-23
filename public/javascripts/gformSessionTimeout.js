(function(global) {
  "use strict";

  var $ = global.jQuery;
  var GOVUK = global.GOVUK || {};
  var lang = (global.gform && global.gform.lang) || "en";

  function GformSessionTimeout() {
    var self = this;

    function init() {
      $("input").keypress(broadcastTimestamp);
    }

    function broadcastTimestamp() {
      if (window.BroadcastChannel) {
        const channel = new window.BroadcastChannel("session-activity");
        console.log("send session timeout1")
        channel.postMessage({
          timestamp: Date.now()
        });
      }
      return true;
    }

    self.GformSessionTimeout = function() {
      init()
    }
  }

  GformSessionTimeout.prototype.init = function() {
    this.GformSessionTimeout();
  };

  GOVUK.GformSessionTimeout = GformSessionTimeout;
  global.GOVUK = GOVUK;
})(window);
