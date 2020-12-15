# EatUp README


This project contains a game-like web application that requires
participants to view a series of photos of arthropods, and decide, for
each one, whether or not it is an ant.  While this game is complete
(except for Firebase database credentials), it is highly likely that
changes to the game will be required for other uses, which will
require web programming skills.

The game starts with an introductory page, `index.html`. It introduces
the game and takes the player to an online consent form,
`consent.html`. If the player consents, they are taken to `start.html`
and have to state whether they have played the game before.  The game
itself is `trial.html`. Once the game is complete, the
page `finish.html` is displayed.

The game will not work if viewed from a file (due to browser security
restrictions). For development purposes, you can obtain a basic free
web server such as [Mongoose](https://cesanta.com/). Simply copy it
into this directory and run it. However, for people to play the game,
you will need to use an Internet accessible web server.

The game is implemented using HTML, CSS and JavaScript. User responses
are stored in a Google Firebase database. Images of animals are in the
`images` subdirectory. To run the game using Firebase as your
database, you will need to obtain an account and enter its information
in `mfb.js`. If you use some other database or mechanism to store user
responses, you will need to implement and initialise a new logger (see
`logger.js` and `setup.js`).

Many parts of this directory are specific to an implementation,
however they have been left here to serve as an example or a starting
point for future implementations. As an absolute minimum, it would be
necessary to change (or remove) all of the pages except `trial.html`,
replace the photos in `images`, and set up a Firebase account.

Our particular configuration, with static web pages stored on Github
and a Google Firebase database, suffers from a security
problem. Either the database must be publically writable, or else the
credentials must be publically accessable on Github. This is clearly
risky, so should be avoided.