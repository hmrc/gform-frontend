(function(global) {
  "use strict";

  function GformSessionTimeout() {
    var self = this;

    var enableBroadcastTimestamp = true;
    var signOutUrl = "";
    function init() {
      $("input").keypress(broadcastTimestamp);
      $("textarea").keypress(broadcastTimestamp);
      signOutUrl = $('meta[name="hmrc-timeout-dialog"]').attr('data-sign-out-url');
    }

    function broadcastTimestamp() {
      if (!enableBroadcastTimestamp) return;
      enableBroadcastTimestamp = false;
      setTimeout(function () { enableBroadcastTimestamp = true; }, 60000)
      if (window.BroadcastChannel) {
        const channel = new window.BroadcastChannel("session-activity");
        channel.postMessage({
          timestamp: Date.now()
        });
        $.ajax({
            url: "/submissions/keep-alive",
            type: "GET",
            success: function(data, textStatus, xhr) {
              if (xhr.status == 403) {
                  window.location.href = signOutUrl
              }
            }
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
