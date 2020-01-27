---
title: "EatUp README"
output: html_document
---


This project contains a game-like web application that requires
participants to view a series of photos of arthropods, and decide, for
each one, whether or not it is an ant.

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
`images` subdirectory. If you use Firebase as your database, enter its
information in `mfb.js`. If you use some other database, you will need
to implement and initialise a new logger (see `logger.js` and
`setup.js`).

Many parts of this directory are specific to an implementation,
however they have been left here to serve as an example or a starting
point for future implementations. As an absolute minimum, it would be
necessary to change (or remove) `index.html`, `start.html`, 