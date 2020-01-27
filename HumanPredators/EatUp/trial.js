"use strict";
/* Trial logic HTML assumptions: 
The img element:
  id = configurable
  class = "sample"
  While an image is loading, it (temporarily) has class "loading". 
  When the image has been scored, the score name (e.g. "ant") is added to the class list.

The score buttons:
  class = "score"
  id = score value (e.g. "ant")

When the trial is finished, the URL is set to "finish.html".
*/

// =======================
// General functions

// General function to insert a DOM node after an existing node
function insertAfter(newNode, referenceNode) {
    referenceNode.parentNode.insertBefore(newNode, referenceNode.nextSibling);
}

// General function to remove a DOM node
function removeElement(element) {
    var par = element.parentNode;
    if (par != null)
        par.removeChild(element);
}

function GetUrlParam(name) {
    return new URLSearchParams(window.location.search).get(name);
}

// =========================

// A class which controls the logic of a trial
class Trial {
    // Creates a new Trial instance.
    // @constructor
    // @param {Logger} logger
    // @param {PhotoSeq} photos the photo pool.
    // @param (string) photoEleId ID of the DOM IMG element that displays photos.
    // @param {number} escapeTimeout Duration (milliseconds) before animals escape. If <= 0, animals don't escape.
    // @param {number} animationDuration the animation duration
    //                 (milliseconds) so we can use setTimeout rather than
    //                 transitionend events.
    constructor(logger, photos, photoEleId, escapeTimeout, animationDuration) {
        this.logger = logger;
        this.photos = photos;
        this.escapeTimeout = escapeTimeout;
        this.allowEscape = escapeTimeout > 0;
        this.photoEleId = photoEleId;
        this.animationDuration = animationDuration;
        
        this.startTime = 0;
        this.timeoutId = null;
        this.timerId;
        this.mistakes = [];
        this.totalScored = 0;
        this.buttonsDisabled = false;
    }

    // Gets the game ready to run. Displays the first photo, sets up
    // score button handlers and keyboard shortcuts, and starts the
    // timer used to trigger escapes and record user decision times.
    prepare(shortcutKeys) {
        // Display the first photo
        document.getElementById(this.photoEleId).setAttribute("src", this.photos.currentPhoto.url);

        // Setup click event handlers on buttons
        var scoreBtns = document.querySelectorAll(".score");
        var self = this;
        scoreBtns.forEach(function(elem) {
            elem.addEventListener("click", (e) => {
                self.userScore(e.currentTarget.id);
            })
        });
        
        // Setup keyboard shortcut keys
        function handleKey(e) {
            var score = shortcutKeys[e.key];
            if (typeof(score) !== typeof undefined) {
                self.userScore(score);
            }
        }
        document.addEventListener("keydown", handleKey);

        // Start the countdown timer
        this.startTiming();
    }


    // =======================
    // Game functions

    // "Disable" the "Ant" and "Non ant" buttons.
    // They aren't really disabled, but scoring doesn't function while they are disabled
    disableButtons() {
        //console.log("DISABLE");
        this.buttonsDisabled = true;
    }

    // "Enable" the "Ant" and "Non ant" buttons.
    enableButtons() {
        this.buttonsDisabled = false;
    }

    /** Loads a new photo. A new image element is created, its src
        attribute is set to the specified url, and its class is set to
        self.photoEleId and "loading". Once the image has been loaded, the
        element is added to the document, its ID set to "sample",
        "loading" is removed from its class, and the countdown timer is
        started. */
    loadPhoto(url) {
        var img = new Image;
        var self = this;
        img.onload = function () {
            var img = this;
            var old = document.getElementById(self.photoEleId);
            // Clear ID on old element, and set it on the new element
            old.id = null;
            img.id = self.photoEleId;
            insertAfter(this, old);
            // Calling setTimeout is an ugly hack to get the display to animate. It can fail
            setTimeout(function() {img.classList.remove("loading"); }, 100);
            function photoDisplayed(e) {
                //img.removeEventListener("transitionend", photoDisplayed, false);
                self.enableButtons();
                // New image is now displayed, get rid of the old one
                removeElement(old);
            }
            // Another ugly thing - CSS animations are not run when the
            // tab is inactive, so use a timeout rather than transitionend
            // event to ensure the buttons are always enabled. Hard-wire
            // the animation duration
            //img.addEventListener("transitionend", photoDisplayed, false);
            setTimeout(photoDisplayed, self.animationDuration);
            self.startTiming();
        };
        img.className = this.photoEleId + " loading";
        img.src = url;

        
        // Show progress through list of photos
        var bar = document.getElementById("bar");
        if (bar)
            bar.style.width = Math.min(100, this.photos.percentComplete) + "%";
    }

    showTimeInSecs(secs) {
        document.getElementById("time").innerHTML = Math.abs(this.escapeTimeout / 1000 - secs).toFixed(1);
    }

    // Starts the timer, and sets a timeout to score the current image as
    // "escape" once the timeout has elapsed.
    startTiming() {
        this.startTime = Date.now();
    
        // Update timer regularly
        var self = this;
        this.timerId = setInterval(function() {
            var nMilliSecs = Date.now() - self.startTime;
            self.showTimeInSecs(nMilliSecs / 1000);
        }, 20);

        function timeoutFired() {
            self.timeoutId = null;
            if (self.allowEscape)
                self.userScore("escape");
        }
        this.timeoutId = setTimeout(timeoutFired, this.escapeTimeout);
    }

    // Stops the timer
    stopTiming() {
        var nMilliSecs = Date.now() - this.startTime;
        clearInterval(this.timerId);
        return nMilliSecs;
    }

    // Called when there are no more images to be scored. Saves the total
    // time in a cookie, and displays the finish page
    trialFinished() {
        setCookie("totalTime", this.logger.totalElapsed);
        setCookie("totalScored", this.totalScored);
        setCookie("errors", JSON.stringify(this.mistakes));
        // Save the session ID so we can optionally report user's results if we decide that's a good idea
        setCookie("sessionId", this.logger.sessionId);
        // Browse to the finish page
        window.location = "finish.html"; 
    }
    
    // Records the user's classification for the current image.  Animates
    // the current image away, and loads the next image.
    userScore(score) {
        // There's a short potential race condition - while the new photo
        // is loading, we could accidentally re-score the old photo. To
        // avoid this, we "disable" the buttons while a photo is loading,
        // and if the user scores during that time, we just ignore it
        if (this.buttonsDisabled) {
            //console.log("Ignoring score, buttons disabled: " + this.photos.currentPhoto.url + ", " + score  + ", " + nMilliSecs);
            return false;
        }
        
        // Cancel the timeout since the image has been scored
        if (this.timeoutId) {
            clearTimeout(this.timeoutId);
            this.timeoutId = null;
        }
        
        if (!this.photos.hasCurrentPhoto) {
            //console.log("Skipping score - no current photo (" + this.photos.index + " of " + this.photos.numToShow + ")");
            return false;
        }
        
        this.disableButtons();
        
        var nMilliSecs = this.stopTiming();
        this.logger.logImageScore(this.photos.currentPhoto.url, score, nMilliSecs);
        
        this.totalScored++;
        // Record mistakes
        if (score != this.photos.currentPhoto.correctScore)
            this.mistakes.push(this.photos.currentPhoto.url);
        
        // Move on to the next image
        var morePhotos = this.photos.moveToNext;
        var ie = document.getElementById(this.photoEleId);
        ie.classList.add(score);
        var self = this;
        function onTransEnd(e) {
            // Hide it, but don't remove it because it's a place holder for the next image
            ie.style.display = "none";
            if (!morePhotos)
                self.trialFinished();
        }
        // Don't use transitionend since it doesn't fire if tab is inactive
        //ie.addEventListener("transitionend", onTransEnd, false);
        setTimeout(onTransEnd, this.animationDuration);
        
        // Load next image
        if (morePhotos) {
            this.loadPhoto(this.photos.currentPhoto.url);
        }
        
        return true;
    }
}
