newlisp-matrix-client
=====================

This is a client library for [matrix.org](http://matrix.org), mainly meant as the basis for bots.

Status
------

This project is in early development.

This is my first newlisp project, so the style may change at times.

Some code is experimental.


Example Usage
-------------
    (load "matrix.lsp")
    (poll-events filt-messages)

This starts polling the event-stream, and prints out all messages to the console.


Branching model
---------------

master is the development branch.
Other development branches may be opened.
Once there's something release worthy, a release branch will be opened.

