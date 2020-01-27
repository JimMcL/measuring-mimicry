"use strict";
// Some functions which are probably specific to my application
// Depends on papaparse.js (or better yet, papaparse.min.js).


// Returns true if we are in debug mode. Debug mode is invoked if: (1)
// the document is a local file (i.e. protocol == "file:", or (2) the
// URL has a query parameter "debug=T"
function IsDebugging() {
    return GetUrlParam("debug") == "T" || window.location.protocol == "file:";
}

// Chooses (and returns) a logger. Normally it will be a Firebase
// logger, but when debugging, it will be a console logger.  
//
// If running in debug mode, the "#watermark" style.display is cleared
function ChooseLogger(debug) {
    // Get the data logger
    var logger = null;
    if (debug) {
        // This logger just writes to the console
        logger = new ConsoleDataLogger(GetUserId());
        // Display debug symbol
        var ele = document.getElementById("watermark");
        if (ele)
            ele.style.display = "";
    } else {
        // This logger writes to a Firebase database
        InitFirebase();
        logger = new FirebaseLogger(GetUserId());
    }
    return logger;
}

function ReadPhotosCSV(photosCsvUrl, callback) {
    Papa.parse(photosCsvUrl, {
	download: true,
        header: true,
        skipEmptyLines: true,
        dynamicTyping: true,
	complete: function(results) {
            if (results.errors.length > 0) {
                alert("Error retrieving photos to be displayed: " + results.errors[0].message + ", row " + results.errors[0].row);
            } else {
                // Call the callback
                callback(results.data);
            }
	},
        error: function()
	{
	    // executed if an error occurs while loading the file,
	    // or if before callback aborted for some reason

            // This is not a very good way to report an error
            //alert("Error retrieving photos to be displayed");
	}
    });
}

// Downloads the photo list and starts the trial.
// Assumes that papaparse is loaded.
function PrepareAndStartTrial(logger, photosPerTrial, photosCsvUrl, photoEleId, escapeTimeout, animationDuration, shortcutKeys) {

    // Invoked once we have the list of photos
    function prepare(data) {
        // Derive columns required by the PhotoSeq constructor
        var candidatePhotos = data.map(n => {
            n.correctScore = n.family == "Formicidae" ? "ant" : "notAnt";
            n.url = n.webUrl;
            // Ants seem to weight others, so weight them down a little
            n.weight = n.correctScore == "ant" ? 0.85 : 1;
            return n;
        });

        // Prepare the photo list from the candidate photos
        var photos = new PhotoSeq(candidatePhotos, photosPerTrial);

        // Start the trial
        new Trial(logger, photos, photoEleId, escapeTimeout, animationDuration).prepare(shortcutKeys);
    }

    ReadPhotosCSV(photosCsvUrl, prepare);
}
