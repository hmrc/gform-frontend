(function(global) {
  "use strict";

  function GformSessionTimeout() {
    var self = this;

    var enableBroadcastTimestamp = true;
    var signOutUrl = "";
    var keepAliveUrl = "";
    function init() {
      $("input").keypress(broadcastTimestamp);
      $("textarea").keypress(broadcastTimestamp);
      signOutUrl = $('meta[name="hmrc-timeout-dialog"]').attr('data-sign-out-url');
      keepAliveUrl = $('meta[name="hmrc-timeout-dialog"]').attr('data-keep-alive-url');
      enableBroadcastTimestamp = $('meta[name="hmrc-timeout-dialog"]').length !== 0
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
            url: keepAliveUrl,
            type: "GET",
            error: function(xhr, textStatus, errorThrown) {
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
