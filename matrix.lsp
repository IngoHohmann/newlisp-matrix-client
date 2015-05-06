;(load "version.lsp")
;(println (main-args))
;
; defaults
;

(set 'autosave true)
(set 'autoload true)
(set 'debug-me false)
(constant '*settings-file* "data.settings.lsp")
(constant '*data-file*     "data.data.lsp")
(constant '*my-file*       "data.my.lsp")

;
; print debug info
;
(define (*** val str)
	"print info if debug-me is true"
	(if debug-me (println "# " (if str (string str ": ") "") (or val ".")))
	val)

;
; default settings
;
(define settings:settings)
(settings "protocol" "http://")
(settings "host-name" "localhost")
(settings "port-id" 8008)
(settings "base-path" "_matrix/client/api/v1/")
(set 'host "")

;
; data contexts
;
(*** "data contexts")

(define db:db)
(define data:data)
(define my:my)

t stat(define (save-my force)
	"save my data, use force when autosave is not enabled"
	(if (or autosave force)
		(save *my-file* 'my)))

(define (save-settings force)
	"save settings data, use force when autosave is not enabled"
	(if (or autosave force)
		(save *settings-file* 'settings)))

(define (save-data force)
	"save data, use force when autosave is not enabled"
	(if (or autosave force)
		(save *data-file* 'data)))

(define (load-my)
	(catch (begin (load *my-file*)(*** "my data loaded")) 'error))

(define (load-settings)
	(catch (begin (load *settings-file*)(*** "settings loaded")) 'error))

(define (load-data)
	(catch (begin (load *data-file*)(*** "data loaded")) 'error))

;
; change settings
;
(define (set-server host-name port-id protocol base-path)
	"set all host settings in one, skip with nil"
	;TODO: save multiple server settings
	(if host-name (settings "host-name" host-name))
	(if port-id (settings "port-id" port-id))
	(if protocol (settings "protocol" protocol))
	(if base-path (settings "base-path" base-path))
	(settings "host" (string (settings "protocol") (settings "host-name") ":" (settings "port-id") "/" (settings "base-path")))
	(set 'host (settings "host"))
	(save-settings)
	host)

(define (set-server0)
	(settings "host" (string (settings "protocol") (settings "host-name") ":" (settings "port-id") "/" (settings "base-path")))
	(set 'host (settings "host"))
	(save-settings))

;
; load data
;

(load-settings)
(load-my)
(load-data)

; now construct the host
(set-server)


;
; Settings
;
(*** "Settings")

(define (my-user-id)
	"return my user-id"
	(my "id"))

(define (my-access-token)
	"return url access token string"
	(append "?access_token=" (my "token")))

(define (my-room-display)
 "display local-part of current-room"
 (if (my "room")
	(local-part (first (lookup "aliases" ((my "rooms")(first (ref (my "room") (my "rooms")))))))
	""))

(define (disp-room room_id)
	"incomplete don't use"
	(let ((pos (first (ref room_id (my "rooms")))))
		(first (guard (lookup "aliases" ((my "rooms") pos))))))

(define (display-room room_id)
	"Get a displayable string from a room_id, otherwise return room_id"
	(let ((room 
		(cascade 
			(my "rooms")
			(ref room_id value)
			(first value)
			((my "rooms") value))))
		(or
			(and  
				(true? room) 
				(or 
					(if (!= "" (lookup "name" room)) (lookup "name" room)))
					(cascade room
						(lookup "aliases" room)
						(first value)))
			room_id)))

(define (test-room)
	(let (room (lookup "aliases" ((my "rooms") (first (ref "!KfEMRrLQmcjCxwNgXU:localhost" (my "rooms"))))))
		(0 (find ":" room) room)))
		
(define (protocol new-prot)
	"change the server protocol part"
	(unless (ends-with new-prot "://")
		(extend new-prot "://"))
	(settings "protocol" new-prot)
	(set-server))

(define (server new-host)
	"change the server host part"
	(settings "host-name" new-host)
	(set-server))

(define (port-id new-port-id)
	"change the server port-id part"
	(settings "port-id" new-port-id)
	(set-server))

(define (base-path new-base-path)
	"change the server api path"
	(settings "base-path" new-base-path)
	(set-server))

;
; Color
;
;(context 'TermColor)
;;; define some terminal color
; Taken from nl-units.lsp
(constant '+fg-light-red+     "\027[1;31m")
(constant '+fg-light-green+   "\027[1;32m")
(constant '+fg-light-yellow+  "\027[1;33m")
(constant '+fg-red+           "\027[31m")
(constant '+fg-green+         "\027[32m")
(constant '+fg-yellow+        "\027[33m")
(constant '+bg-cyan+          "\027[46m")
(constant '+bg-dark-gray+     "\027[1;40m")
(constant '+reset+            "\027[0m")
;;; colorize a string
(define (colorize color str)
	(letn (
			(color-name (term color))
			(const-str (append "+" color-name "+"))
			(color-sym (sym const-str)))
		(append (eval color-sym) str +reset+)))


;
; Helpers
;
(*** "Helpers")

(define (help fun) 
	"show help info" 
	(when (lambda? fun)
		(and 
			(string? (nth 1 fun))
			(println "\nhelp text: " (nth 1 fun)))
		(println    "arguments: " (nth 0 fun) "\n")
		true))

(define (walk-tree tree (check (lambda())))
	"walks the tree and performs check on all values"
	(check tree)
	(if (list? tree)
		(dolist (el tree) 
			(walk-tree el check))
		))

(define (assoc-deep key alist)
	"uses ref to find an assoc deep in a tree"
	(let ((idx (ref key alist)))
		(if idx
			(alist (chop idx))
			(list key ""))))

(define (lookup-deep lst)
	"uses ref for a deep lookup, iterating over lst into depth"
	(let ((tmp lst)(pos '()))
		(dolist (val (args) (nil? pos))
			(and
				(set 'pos (ref val tmp))
				(set 'tmp (tmp (push (+ 1 (pop pos -1)) pos -1)))))
		tmp))

(define (upto sub-string string)
	"returns first part of a string upto a substring"
	;TODO
	(if (all sub-string string)
		(let (pos (find sub-string string))
			(if pos
				(slice 0 )))))

(define-macro (preset symb default-value)
	"set symbol to default-value, if it currently is nil"
	(set symb (or (eval symb) (eval default-value))))

(define (local-part val)
	"returns the local part of a user-id / room alias or empty string"
	(if 
		(and
			(string? val)
			(regex ".*:" val))
		(chop $0)
		""))

(define (room-id (name ""))
	"lookup room-id from alias, alias will be extended with # and local part"
	(if 
		(= "" name) ""
		(starts-with name "!") name
		(begin
			(unless (starts-with name "#") 
				(push "#" name))
			(unless (find ":" name)
				(extend name ":" (settings "host-name")))
			; TODO: search in public rooms?
			(local (tmp)
				(or
					(and 
						(set 'tmp (my "rooms"))
						(set 'tmp (ref name tmp))
						(set 'tmp (first tmp))
						(lookup "room_id" ((my "rooms") tmp)))
					nil)
					))))

(define (user-id (name ""))
	"create a locally qualified user-id from only a local part"
	(if 
		(= "" name) ""
		(begin
			(unless (starts-with name "@") 
				(push "@" name))
			(unless (find ":" name)
				(extend name ":" (settings "host-name")))
			name)))

;
; check return value
;
(*** "Check return value")

(define-macro (check-return json if-ok if-not-ok)
	"checks if returned json object is an error object, and performs respective actions"
	(let (json (eval json))
		(if (lookup "errcode" json)
			(eval if-not-ok)
			(eval if-ok))))

(define (guard val (with '()))
	"if val is nil, returns empty list or with-value"
	(or val with))

(define-macro (cascade value)
	"cascade value thru several calls, use value to access current value, stops if value is nil"
	(set 'value (eval value))
	(if value
		(doargs (x (nil? value)) 
			(set 'value (eval x ))))
	value)

;
; json creation
;
(*** "JSON creation")

(define (json-object lst)
	"creates a json object string from a list of associations"
	;(println lst)
	(if (string? lst)
		lst
		(let (json "{")
			(dolist (el lst)
				(if (list? el)
					(begin
						(extend json (string "\"" (el 0) "\":"))
						(if
							(string? (el 1)) (extend json "\"" (el 1) "\",")
							(number? (el 1)) (extend json (string (el 1)) ",")
							(symbol? (el 1)) (extend json "\"" (string (el 1)) "\","))
						)))
			(extend (chop json) "}")
		)))

(define (error-json errcode comment)
	"create an error json string"
	(join (list "{\"errcode\":\"" errcode "\",\"error\":\"" comment "\"}")))

(define (error errcode comment)
	"create an error s-expression"
	(list (list "errcode" errcode)(list "error" comment)))

(define (register-json user pass) 
	(join (list "{" {"type":"m.login.password","user":"} user {","password":"} pass {"} "}")))

(define (login-json user pass) 
	(join (list "{" {"type":"m.login.password","user":"} user {","password":"} pass {"} "}")))

(define (message-json msg (type "m.text"))
	(join (list "{\"msgtype\":\"" type "\",\"body\":\"" msg "\"}")))

(define (displayname-json name)
	(join (list "{\"displayname\":\"" name "\"}")))

(define (avatar-json url)
	(join (list "{\"avatar_url\":\"" url "\"}")))

(define (create-room-json alias public name topic invite)
	(append (chop (append "{"
		(if alias (join (list "\"room_alias_name\":\"" alias "\",")) "")
		(if public
			(append "\"visibility\":\"public\",")
			(append "\"visibility\":\"private\","))
		(if name   (append "\"name\":\"" name "\",") "")
		(if topic  (append "\"topic\":\"" topic "\",") "")
		(if invite (append "\"invite\":[" "],") "")))
		"}"))

(define (room-name-json name)
	(string "{\"name\":\"" name "\"}"))

(define (room-topic-json topic)
	(string "{\"topic\":\"" topic "\"}"))


(define (invite-json user_id)
	(unless (starts-with user_id "@")
		(push "@" user_id))
	(unless (find ":" user_id)
		(append user_id ":" "localhost"))
	(append "{\"user_id\":\"" user_id "\"}"))

(define (friend-invite-json id)
	(string "{\"invite\":[\"" id "\"]}"))

(define (friend-drop-json id)
	(string "{\"drop\":[\"" id "\"]}"))

(define (presence-json state msg)
	(let (json "")
		(extend json (string "{\"presence\":\"" (guard state "") "\",\"status_msg\":\"" (guard msg "") "\"}"))
		json))

(define (add-alias-json id)
	(string "{\"room_id\":\"" id "\"}"))

(define (typing-json state timeout)
	(string "{\"typing\":" state ",\"timeout\":" timeout "}"))

;
; Check return value for json data
;
(*** "check for json")

(define (return-json return)
	"parses json data, or returns an error 'object', if no json data found"
	(if (or (starts-with return "{")(starts-with return "["))
		(json-parse return)
		(let (pos (find "{" return))
			(if (nil? pos)
				(list (list "errcode" "ERROR")(list "error" return))
				(json-parse (slice return (find "{" return)))))))

;
; network connection
;
(*** "network connection")

(define (api function no-token)
	"create api url"
	(if no-token
		(join (list host function))
		(join (list host function "?access_token=" (my "token")))))

(define (api-get path no-access-token)
	(return-json (get-url (api path no-access-token) 3000)))

(define (api-post path (json "") no-access-token) 
	(return-json (post-url (api path no-access-token) json)))

(define (api-put path (json "") no-access-token)
	(return-json (put-url (api path no-access-token) json)))

(define (api-delete path (json "") no-access-token)
	(return-json (delete-url (api path no-access-token) json)))

;
; Registration and login
;
(*** "Registration & login")

(define (do-register user pass)
	(if (nil? user) 
		(return-json (get-url (api "register")))
		(let (user (api-post "register" (register-json user pass) true))
			(unless (lookup "errcode" user)
				(my "id"    (lookup "user_id" user))
				(my "token" (lookup "access_token" user))
				(save-my))
			user)))

(define (do-login user pass)
	(if (nil? user) 
		(api-get "login" true)
		(let (user (api-post "login" (login-json user pass) true))
			(unless (lookup "errcode" user)
				(my "id" (lookup "user_id" user))
				(my "token" (lookup "access_token" user))
				(save-my))
			user)))

(define (register user pass)
	(if (ref '("stages" ("m.login.password")) (do-register))
		(do-register user pass)
		(error "M_NOT_SUPPORTED" "m.login.password is not supported by server")))

(define (login user pass)
	(if (ref "m.login.password" (do-login))
		(do-login user pass)
		(error "M_NOT_SUPPORTED" "m.login.password is not supported by server")))

;
; Profile
;
(*** "Profile")

(define (display-name name)
	"get or set own displayname"
	(if name
		(check-return (api-put (append "profile/" (my-user-id) "/displayname") (displayname-json name))
			(my "displayname" name))
		(check-return (api-get (append "profile/" (my-user-id) "/displayname"))
			(my "displayname" (lookup "displayname" json))))
	(save-my)
	(my "displayname"))

(define (get-display-name user_id)
	"get a users displayname"
	(if user_id
		(api-get (append "profile/" user_id "/displayname"))
		(error "M_NO_ID" "User ID is missing")))

(define (avatar url)
	"get or set own avatar-url"
	(if url
		(api-put (append "profile/" (my-user-id) "/avatar_url") (avatar-json url))
		(check-return (api-get (append "profile/" (my-user-id) "/avatar_url"))
			(my "avatar_url" (lookup "avatar_url" json)))))

(define (get-avatar-url user_id)
	"get a users avatar-url"
	(if user_id
		(api-get (append "profile/" user_id "/avatar_url"))
		(error "M_NO_ID" "User ID is missing")))

(define (presence state msg)
	"get or set own presence status"
	(if (or state msg)
		(api-put (string "presence/" (my-user-id) "/status") (presence-json state msg))
		(api-get (string "presence/" (my-user-id) "/status"))))

(define (get-presence user-id)
	"get presence status for user-id"
	(api-get (string "presence/" user-id "/status")))

(define (friends id)
	(api-get (string "presence/list/" (if id id (my-user-id)))))

(define (friends-invite id)
	(api-post (string "presence/list/" (my-user-id)) (friend-invite-json id)))

(define (friends-drop id)
	(api-post (string "presence/list/" (my-user-id)) (friend-drop-json id)))

;
; Rooms
;
(*** "Rooms")

; creation and setting
(define (create-room alias public name topic invite)
	"create a new room"
	(api-post "createRoom" (create-room-json alias public name topic invite)))

(define (room-name name (room-id (my "room")))
	"set room name of given or current room"
	;(unless room-id (set 'room-id (my "room")))
	(if name
		(api-put (string "rooms/" room-id "/state/m.room.name") (room-name-json name))
		(api-get (string "rooms/" room-id "/state/m.room.name"))))
		
(define (room-topic topic room-id)
	"set room topic of given or current room"
	(unless room-id (set 'room-id (my "room")))
	(if name
		(api-put (string "rooms/" room-id "/state/m.room.topic") (room-topic-json topic))
		(api-get (string "rooms/" room-id "/state/m.room.topic"))))

(define (add-alias alias id)
	(api-put (string "directory/room/" alias) (add-alias-json id)))

(define (delete-alias alias)
	(api-delete (string "directory/room/" alias)))

(*** "list rooms")

(define (my-rooms)
	"list all rooms I have joined"
	(my "rooms" '())
	(check-return (initial-sync 0)
		(begin
			(dolist (room (guard (lookup "rooms" json)))
				(let (thisroom '())
					(push (assoc-deep "aliases" room) thisroom)
					(push (assoc-deep "topic" room) thisroom)
					(push (assoc-deep "name" room) thisroom)
					(push (list "start" (lookup-deep room "messages" "start")) thisroom)
					(push (list "end" (lookup-deep room "messages" "end")) thisroom)
					(push (assoc "room_id" room) thisroom)
					(extend (my "rooms") (list thisroom)))) 
			(save-my) 
			(my "rooms"))))
		
(define (public-rooms)
	"return all public rooms"
	(check-return (return-json (get-url (api "publicRooms")))
		(set 'db:rooms (lookup "chunk" json))))

(define (room-members room-id from to limit)
	"list room members"
	; TODO from to limit
	(unless room-id (set 'room-id (my "room")))
	(api-get (string "rooms/" room-id "/members")))

(*** "read events")

(define (initial-sync (limit 0))
	"do an initial sync"
	; add access-token by hand, because we already have "?limit="
	(api-get (string "initialSync?limit=" limit "&access_token=" (my "token")) true))
	;(api-get (string "initialSync?limit=" limit))); "&access_token=" (my-access-token)) true))

(define (room-sync id (limit 0))
	"do an initial sync inside a room"
	(api-get (string "rooms/" id "/initialSync?limit=" limit "&access_token=" db:access_token) true))

(*** "join & leave")

(define (join-room room)
	"join a room"
	(let (room (api-post (append "join/" room ) ""))
		(unless (lookup "errcode" room)
			(my "room" (lookup "room_id" room))
			(my "room-info" room))
		room))

(define (leave-room (room-id (my "room")))
	"leave room"
	(let (room (api-post (append "leave/" room-id) ""))
		(unless (lookup "errcode" room)
			(my "room" (lookup "room_id" room))
			(my "room-info" room))
		room))

(define (enter room_id)
	"set room as current-room"
	(if room_id
		(if (room-id room_id) 
			(my "room" (room-id room_id))
			(error "M_NOT_FOUND" "unable to find room"))
		(error "M_NO_ROOM_ID" "room-id is missing")))

(define (invite user (room-id (my "room")))
	"invite a user to a room"
	(api-post (string "rooms/" room-id "/invite" ) (invite-json user)))

(*** "events & state")

(define (send-event event-body event-type (room-id (my "room")))
	(api-post (string "rooms/" room-id "/send/" event-type) (json-object event-body)))

(define (set-state event-body event-type state-key (room-id (room-id)))
	(api-put (string "rooms/" room-id "/state/" event-type "/" state-key) (json-object event-body)))

(define (send-message message (msgtype "m.text") (type "m.room.message") (room_id (my "room")) json-string)
	"send different types of messages"
	(replace message "\n" "\\n")
	(set 'room_id (or room_id (my "room")))
	(set 'type (or type "m.room.message"))
	(set 'msgtype (or msgtype "m.text"))
	(unless json-string
		(set 'json-string (message-json message msgtype)))
	(api-post (string "rooms/" room_id "/send/" type) json-string))

(define (emote message (room_id (my "room")))
	"send an m.emote message"
	(replace message "\n" "\\n")
	(unless room_id (set 'room_id (my "room")))
	(send-message message "m.emote" "m.room.message" room_id))

(define (notice message (room_id (my "room")))
	"send an m.emote message"
	(replace message "\n" "\\n")
	(unless room_id (set 'room_id (my "room")))
	(send-message message "m.notice" "m.room.message" room_id))

(define (message message (room_id (my "room")))
	"Send a text message to the room or current-room"
	(replace message "\n" "\\n")
	(unless room_id (set 'room_id (my "room")))
	(api-post (append "rooms/" room_id "/send/m.room.message") (message-json message)))

(define (messages room-id from to limit backwards)
	"read message stream, return m.room.message messages"
	(let ((query "") (json nil) (ret '()))
		(if from (extend query (string "&from=" from)))
		(if to 
			(if (= 'back to)
				(set 'backwards true) ;(extend query "&dir=b")
				(extend query (string "&to" to))))
		(if limit (extend query (string "&limit" limit)))
		(if backwards (extend query "&dir=b"))

		(unless room-id (set 'room-id (my "room")))
		(check-return (api-get (string "rooms/" room-id "/messages" (my-access-token) query) true)
			(begin
				; set pagination info

				; build return value
				(push (assoc "start" json) ret)
				(push (assoc "end" json) ret)
				(dolist (m (lookup-deep json "chunk")) 
					(if (= "m.room.message" (lookup "type" m))
						(push m ret))))
			(set 'ret json))
		ret))

(define (state (room-id (my "room")))
	"get the current state of given or current room"
	(unless room-id (set 'room-id (my "room")))
	(api-get (string "rooms/" room-id "/state")))

(define (members room-id)
	(unless room-id (set 'room-id (my "room")))
	(api-get (string "rooms/" room-id "/members")))

(define (typing (room (my "room")) (state true) (timeout 10))
	(api-put (string "rooms/" room "/typing/" (my "id")) (typing-json state timeout)))

(*** "polling")

(define (events from timeout)
	(let ((query ""))
		(if from 
			(set 'query (string "&from=" from)))
		(if timeout
			(append query ("&timeout=" timeout)))
		(api-get (string "events?access_token=" (my "token") query) true)))

(***)

(define (poll-events filter-func)
	"listen for events, run filter-func on received events (DEFAULTS to printing events)"
	(let ((from (my "sync-from")))
		(unless filter-func (set 'filter-func (fn (js) (println "-> " js))))
		(while true 
			(check-return (events from)
				(begin
					(set 'from (lookup "end" json))
					(my "sync-from" from)
					(save-my)
					(filter-func json))))))

(define (msg-indicator evt)
	"Print message indicator for text / emote / notice"
	(case (lookup-deep evt "msgtype")
		("m.text" "  ")
		("m.emote" (string "* " (lookup "user_id" evt) " "))
		("m.notice" "! ")
		(true "??")))

(define (filt-messages js)
	"example event filter, print all messages"
	(dolist (evt (lookup "chunk" js))
		(case (lookup "type" evt)
			("m.room.message" 
				(println "\n" (display-room (lookup "room_id" evt)) " : "
					(lookup "user_id" evt) ":\n" 
					(msg-indicator evt) (lookup-deep evt "content" "body"))))))

(define (filt-all js)
	"exampe event filter, print all events"
	(dolist (evt (lookup "chunk" js))
		(case (lookup "type" evt)
			("m.room.message" 
				(println "\n" (display-room (lookup "room_id" evt)) " -> "
					(lookup "user_id" evt) ":\n" 
					(msg-indicator evt) (lookup-deep evt "content" "body")))
			("m.presence"     
				(println "   >>> " (lookup-deep evt "content" "presence") ":\t" 
				(lookup-deep evt "content" "user_id")))
			("m.typing"       
				(begin
					(print "   >>> typing:\t")
					(dolist (user (lookup-deep evt "content" "user_ids"))
						(print user " "))
					(println "")))
			("m.room.member"
				(println "   >>> " (lookup-deep evt "content" "membership") "\t from " 
				(lookup "user_id" evt) " to " (display-room (lookup "room_id" evt))))
			(true (println js)))))

(***)

(define (event id)
	"get info on event"
	(api-get (string "events/" id)))

(***)

(define (listen room from timeout)
	(let ((query ""))
		(if from 
			(set 'query (string "&from=" from)))
		(if timeout
			(append query (if from "&" "?") "timeout=" timeout))
		(println query)
		(api-get "events")))

(***)

(define (poll)
	(dotimes (i 5)
		(println (events))))

;
; set the prompt to show user / room
;
(*** "set prompt")

(prompt-event 
	(fn (ctx)
		(string (my-user-id) " / " (my-room-display) "> ")))

; send message from command-line
(when (> (length (main-args)) 2)
	(let ((arg (rest (rest (main-args)))))
		(if
			(set 'val (match '("send" ?) arg))    (message val)
			(set 'val (match '("send" ? ? ) arg)) (message (val 1) (room-id (val 0)))) 
		(exit)))

"loaded-successfully"
