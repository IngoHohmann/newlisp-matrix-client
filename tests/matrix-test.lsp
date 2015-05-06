(load "nl-unittest.lsp")

(context 'MAIN)

(load "../matrix.lsp")
(define-test (test_default_host)
	(assert= "http://" (settings "protocol"))
	(assert= "localhost" (settings "host-name"))
	(assert= 8008 (settings "port-id"))
	(assert= "_matrix/client/api/v1/" (settings "base-path"))
	(assert= "http://localhost:8008/_matrix/client/api/v1/" host)
	(assert= "http://localhost:8008/_matrix/client/api/v1/" (settings "host"))
	)

(define-test (test_host_settings)
	(assert= true (protocol "https"))
	(assert= true (server "myhost"))
	(assert= true (port-id 8088))
	(assert= true (base-path "_mat/ap/v1/test"))
	(assert= "https://myhost:8088/_mat/ap/v1/test" (settings "host")))

(define-test (test_reset_host_settings)
	(assert= true (protocol "http"))
	(assert= true (server "localhost"))
	(assert= true (port-id 8008))
	(assert= true (base-path "_matrix/client/api/v1/"))
	(assert= "http://localhost:8008/_matrix/client/api/v1/" (settings "host")))

(define-test (test_login_fail)
	(assert= "ERR: string expected : nil\ncalled from user function login-json\ncalled from user function api-post\ncalled from user function do-login\ncalled from user function login\ncalled from user function assert=" (login))
	(assert= '(("errcode" "M_FORBIDDEN") ("error" "")) (login "xxxtestxxx" "")))

(define-test (test_register)
	(assert= true (register "xxxtestxxx" "testpass")))

(define-test (test_login)
	(assert= true (login "xxxtestxxx" "testpass")))

(define-test (test_display_name)
	(assert= "New Display Name" (display-name "New Display Name"))
	(assert= "New Display Name" (display-name)))

(context 'MAIN)

(UnitTest:run-all 'MAIN)
