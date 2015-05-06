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

    ; set homeserver to connect to, defaults to localhost
    (set-server "matrix.org" "80" "https://")

    ; register a new user, you could use login, if the user already exists
    (register "xxx" "xxxxxx")
    ; a file containing your access_token, among others, has been written to your current dir
    ; on the next start, you are already logged in

    ; listen for the event stream, print all messages in all rooms
    (poll-events filt-messages)
    ; This starts polling the event-stream, and prints out all messages to the console.
    ; Use ctrl+c to stop

    ; join a room
    (join-room "#matrix-dev@matrix.org)

    ; enter a room you have already joined
    (enter "#matrix:matrix.org")
    ; in both cases the room will be the current room

    ; send messages in the current room
    (message "test")
    (emote   "feels great :-)")
    (notice  "... but that may just be me ...")

    ; cli
    ; this will probably change
    newlisp matrix.lsp send "here comes the sun"




Branching model
---------------

So far master is the development branch, but this may change later.
Other development branches may be opened.
Once there's something release worthy, a release branch will be opened.

