;;;
;;; This file is a part of the nl-unittest project, released under
;;; MIT license.
;;;
;;; See the COPYING file for more details.
;;;
;;; Copyright (c) 2011 by Dương "Yang" ヤン Hà Nguyễn <cmpitg@gmail.com>
;;;

;;; This file is not designed to use as a standalone program but in
;;; conjuction with other programs.

;;; need cleaning up!!!!!!!!!!!!
(context 'TermColor)

;;; define some terminal color

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
  (letn ((color-name (term color))
         (const-str (append "+" color-name "+"))
         (color-sym (sym const-str)))
    (append (eval color-sym) str +reset+)))

(context 'UnitTest)

(setq *enable-term-color*       true)   ; use colors in console?
(setq *report-failed*           true)   ; report failed assertions?
(setq *report-passed*           nil)   ; report passed assertions?
(setq *continue-after-failure*  true)
(setq *verbose*                 nil)

;;; current test in a test-case, *cur-test* help tracking a test which
;;; contains other test cases
(setq *cur-test* '())

;;; convert and concat all the arguments into a string and colorize it
;;; if necessary
(define (colorize color)
  (letn (s (apply append (args)))
    (if *enable-term-color*
        (TermColor:colorize color s)
        s)))

(define (report-failure expression)
  (let (report
        (if (true? *verbose*)
            (colorize 'fg-red "--> " (string expression) " FAILED!")
            (colorize 'fg-red
                      ;; "--> Expression: " (string (expression 2))
                      "--> " (string expression)
                      " => Expected: " (string (eval (expression 1)))
                      " -> Received: " (string (eval (expression 2))) ".")))
    (if *report-failed*
        (println report)))
  nil)

(define (report-error expression msg)
  (let (report
        (colorize 'fg-light-yellow "--> " (string expression)
                  " got error(s):\n"
                  (join (map (lambda (s) (append "    " s))
                             (parse msg "\n"))
                        "\n")))
    (println report))
  nil)

;;; requert result of a passed test
(define (report-pass expression)
  (let (report (colorize 'fg-green "--> " (string expression) " passed"))
    (if *report-passed*
        (println report)))
  true)

(define (assertion? form)
  ;; a symbol is an assertion only if it contains "assert="
  (local (res)
    (catch (term (first form)) 'res)
    (ends-with res "assert=")))

(define (report-result expr res)
  ;; (println "-- debug; report-result => " expr " -> " res)
  (if (= true res) (report-pass expr)
      (= nil res) (report-failure expr)
      (report-error expr res)))

;;; run all the test cases and return the list of results
(define (run-and-report cases , res)
  (setq res '())
  (dolist (single-case cases)
    (let (*cur-results* '() *cur-expressions* '())
      (catch (eval single-case) 'some-error)

      ;; now, we have the list current results as well as their
      ;; corresponding expressions: ``*cur-results*`` and
      ;; ``*cur-expressions*``
      (dotimes (idx (length *cur-results*))
        (report-result (*cur-expressions* idx)
                       (*cur-results* idx)))

      (setq res (append res *cur-results*)))))

(define-macro (check test-cases cur-test)
;;  (println)
  (println "=== Testing " (eval cur-test))

  (letn (time-running 0 result-list '())

    ;; calculate result and time at the same time
    (setq time-running
          (time (setq result-list (run-and-report test-cases))))

    ;; because result-list may contain non-assertion expression, we
    ;; need to filter them out
    (letn ((passed-ass (length (filter (lambda (x) (= true x))
                                       result-list)))
           (failed-ass (length (filter (lambda (x) (!= true x))
                                       result-list)))
           (total-ass (+ passed-ass failed-ass))
           (msg-passed (append (string passed-ass) " pass(es)"))
           (msg-failed (append (string failed-ass) " fail(s)")))

      ;; colorize if necessary
      (if (< 0 passed-ass)
          (setq msg-passed (colorize 'fg-light-green msg-passed)))
      (if (< 0 failed-ass)
          (setq msg-failed (colorize 'fg-light-red msg-failed)))

      (println ">>> Total assertions: " total-ass)
      (println "  - " msg-passed)
      (println "  - " msg-failed)
      (println "  - Total time: " time-running "ms")
      (println)

      ;; the test case is considered passed only if there's no failure
      (= 0 failed-ass))))

;;; run all test cases, aka functions of the form ``test_``
(define (run-all cont)
  (println)
  (println "======================================================================")
  (letn (counter 0 failed 0 passed 0 time-running 0)
    (dotree (symbol cont)
      (if (starts-with (term symbol) "test_")
          (begin
            (inc time-running
                 (time (if (apply symbol)
                           (inc passed)
                           (inc failed))))
            (println "----------------------------------------------------------------------"))))

    ;; make report and colorize if necessary
    (setq counter (+ failed passed))

    (println "STATUS:")
    (println "==> RAN " counter " test(s) IN " time-running "ms")
    (println "==> "
             (if (zero? failed)
                 (colorize 'bg-dark-gray
                           (colorize 'fg-green "ALL PASSED!!!"))
                 (colorize 'bg-dark-gray
                           (colorize 'fg-red
                                     "FAILED (failures = "
                                     (string failed) ")"))))
    (println "======================================================================")
    (println)
    (zero? failed)))

;;;
;;; convenient methods in context 'MAIN
;;;

(context 'MAIN)

;;;
;;; alias for ``=`` for testing clarification
;;;
(define-macro (assert= expected expression , _ass-result)
  (catch
      (eval '(apply = (list (eval expected) (eval expression))))
    '_ass-result)
  ;; eval the expression and save the result
  (push _ass-result UnitTest:*cur-results* -1)
  ;; save the expression as symbol
  (push (list 'assert= expected expression)
        UnitTest:*cur-expressions* -1)
  ;; (println "-- debug => " (list 'assert= expected expression) " -> "
  ;;          _ass-result)
  _ass-result)

;;;
;;; what this functions does are
;;;   * setting the current test, tracking if the current test contains other tests
;;;   * evaluating every expression in the current test and return the result
(define-macro (define-test params)
  ;;
  ;; `params`     the function signature
  ;; `test_name`  is the name of the test, equals `(first params)`
  ;; `exps`       is the body of the
  ;;
  (eval (expand '(define signature
                  (let ((UnitTest:*cur-test*
                         (append UnitTest:*cur-test* '(test-name))))
                    (UnitTest:check exps UnitTest:*cur-test*)))
                (list (list 'signature  params)
                      (list 'exps       (args))
                      (list 'test-name  (params 0))))))

;;; make all symbol used for testing available
(global 'assert=)
(global 'define-test)
