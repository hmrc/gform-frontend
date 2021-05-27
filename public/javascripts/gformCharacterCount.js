;(function (global) {
   'use strict'

    global.GOVUKFrontend.CharacterCount.prototype.updateCountMessage = function () {
        var countElement = this.$textarea;
        var options = this.options;
        var countMessage = this.$countMessage;

        // Determine the remaining number of characters/words
        var currentLength = this.count(countElement.value);
        var maxLength = this.maxLength;
        var remainingNumber = maxLength - currentLength;

        // Set threshold if presented in options
        var thresholdPercent = options.threshold ? options.threshold : 0;
        var thresholdValue = maxLength * thresholdPercent / 100;
        if (thresholdValue > currentLength) {
            countMessage.classList.add('govuk-character-count__message--disabled');
            // Ensure threshold is hidden for users of assistive technologies
            countMessage.setAttribute('aria-hidden', true);
        } else {
            countMessage.classList.remove('govuk-character-count__message--disabled');
            // Ensure threshold is visible for users of assistive technologies
            countMessage.removeAttribute('aria-hidden');
        }

        // Update styles
        if (remainingNumber < 0) {
            countElement.classList.add('govuk-textarea--error');
            countMessage.classList.remove('govuk-hint');
            countMessage.classList.add('govuk-error-message');
        } else {
            countElement.classList.remove('govuk-textarea--error');
            countMessage.classList.remove('govuk-error-message');
            countMessage.classList.add('govuk-hint');
        }

        // Update message
        var message = remainingNumber < -1 ? window.gform.messages.genericCharactersTooMany :
            (remainingNumber == -1 ? window.gform.messages.genericCharacterTooMany :
               ((remainingNumber == 0 || remainingNumber > 1) ? window.gform.messages.genericCharactersRemaining :
                    window.gform.messages.genericCharacterRemaining));
        var displayNumber = Math.abs(remainingNumber);

        countMessage.innerHTML = message.format(displayNumber);
    }
})(window);