function setCookie(cname, cvalue, exdays) {
    var d = new Date();
    d.setTime(d.getTime() + (exdays * 24 * 60 * 60 * 1000));
    var expires = "";
    if (exdays) {
        expires = ";expires="+d.toUTCString();
    }
    document.cookie = cname + "=" + cvalue + ";" + expires + ";path=/";
}

function getCookie(cname) {
  var name = cname + "=";
  var ca = document.cookie.split(';');
  for(var i = 0; i < ca.length; i++) {
    var c = ca[i];
    while (c.charAt(0) == ' ') {
      c = c.substring(1);
    }
    if (c.indexOf(name) == 0) {
      return c.substring(name.length, c.length);
    }
  }
  return "";
}

function deleteCookie(cname) {
    setCookie(cname, "", -1);
}

function uniqueID(){
  function chr4(){
    return Math.random().toString(16).slice(-4);
  }
  return chr4() + chr4() +
    '-' + chr4() +
    '-' + chr4() +
    '-' + chr4() +
    '-' + chr4() + chr4() + chr4();
}

// Generates and/recalls a user ID as a cookie.
// Does not store any information about the user.
function GetUserId() {
    var uid = getCookie("UserId");
    //console.log("UserId '" + uid + "'");
    if (uid == "") {
        // No UID. generate one
        uid = uniqueID();
        //console.log("generated id '" + uid + "'");
        setCookie("UserId", uid, 365);
    }
    return uid;
}

function ClearUserId() {
    deleteCookie("UserId");
}

function HasUserId() {
    return getCookie("UserId") != "";
}
