// Data logging to the console or a remote database

// =======================
// Base class - generic data logging, logs to the console.
// Derive a subclass and override _log to log to somewhere else

class ConsoleDataLogger {
    constructor(uid) {
        this.uid = uid;
        this.sessionId = uniqueID();
        this.imageNumber = 1;
        this.totalElapsed = 0;
    }

    // Adds sessionId to obj
    fillIn(obj) {
        var nobj = obj;
        nobj["sessionId"] = this.sessionId;
        return nobj;
    }

    // Adds sessionId to obj, then converts it to a JSON string and returns it
    jsonify(obj) {
        return JSON.stringify(this.fillIn(obj));
    }

    // Jsonifies obj then writes it to the output device.
    // Subclasses may perform additional actions
    _log(obj) {
        console.log(this.jsonify(obj));
    }

    // Saves session info
    logUserSession(noob, screenWidth, screenheight, devicePixelRatio, userAgent) {
        this._log({type: "session",
                   userId: this.uid,
                   firstTime: noob,
                   screenWidth: screenWidth,
                   screenheight: screenheight,
                   devicePixelRatio: devicePixelRatio,
                   userAgent: userAgent});
    }
    
    // Saves the user's classification of an image
    logImageScore(imageUrl, score, time) {
        this.totalElapsed += time;
        this._log({type: "score",
                   imageUrl: imageUrl,
                   imageNumber: this.imageNumber++,
                   score: score,
                   time: time});
    }

    // Saves the user's email address because they have requested to be
    // notified of the research outcomes
    logUserEmail(email) {
        this._log({type: "email",
                   userId: this.uid,
                   email: email});
    }
};

// =======================
// Google Firebase logging

class FirebaseLogger extends ConsoleDataLogger {
    // Firebase must be separately initialised for this to work, see InitFirebase function
    _log(obj) {
        // Add sessionId
        obj = this.fillIn(obj);
        // Add created_at field
        obj.created_at = new Date().toISOString();
        //console.log("FIREBASE: " + this.jsonify(obj));
        firebase.database().ref('mimic-scores').push().set(obj)
            .then(function(snapshot) {
                //console.log('FIREBASE success: ' + snapshot);
                //success(); // some success method
            }, function(error) {
                console.log('FIREBASE error: ' + error);
                //error(); // some error method
            });
    }
};

// =======================
// AdaFruit logging

class AdaFruitLogger extends ConsoleDataLogger {

    // AdaFruit parameters - replace with real values
    get user() { return "user"; }
    get feed() { return "feed"; }
    get ioKey() { return "jkskds jhkldjfh dfklj"; }
    
    _log(obj) {
        //console.log("ADAFRUIT: " + this.jsonify(obj));

        // POST a message to AdaFruit IO
        var http = new XMLHttpRequest();
        var url =  "https://io.adafruit.com/api/v2/" + this.user + "/feeds/" + this.feed + "/data";
        var params = "value=" + encodeURIComponent(this.jsonify(obj));
        http.open('POST', url, true);

        // Send the proper header information along with the request
        http.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
        // API key
        http.setRequestHeader("X-AIO-Key", this.ioKey);

        http.onreadystatechange = function() { //Call a function when the state changes.
            // if(http.readyState == 4 && http.status == 200) {
            //     alert(http.responseText);
            // }
        }
        http.send(params);
    }
};
