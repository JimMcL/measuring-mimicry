// Function to initialise Firebase 
function InitFirebase() {
    // Fill in this information with your Google firebase details.
    // Since this information is public, use  use security rules to protect your data
    var firebaseConfig = {
        apiKey: "XXX",
        authDomain: "XXX.firebaseapp.com",
        databaseURL: "https://XXX.firebaseio.com",
        projectId: "XXX",
        storageBucket: "XXX.appspot.com",
        messagingSenderId: "XXX",
        appId: "XXX"
    };
    // Initialize Firebase
    firebase.initializeApp(firebaseConfig);
}

