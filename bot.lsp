#!/usr/bin/newlisp
;
; Bot using matrix.lsp
;

(load "matrix.lsp")

(define (bot js)
	"bot event filter"
	(dolist (evt (lookup "chunk" js))
		(case (lookup "type" evt)
			("m.room.message" 
				(let ((txt (lookup-deep evt "content" "body"))
						(msgtype (lookup-deep evt "msgtype")))
						; m.text
					(if (= "m.text" msgtype)
						(if 
							(starts-with (lower-case txt) "testbot: info")
								(notice (string "Look here: http://github.com/IngoHohmann/newlisp-matrix-client\n"
									"Sorry, that's all I can say for now ...\n" (lookup "room_id" evt)))
							(starts-with (lower-case txt) "§ping")
								(notice "§pong" (lookup "room_id" evt))
						))))
			("m.presence"     
				)
			("m.typing"       
				)
			("m.room.member"
				)
			(true ))))   

(poll-events bot)
