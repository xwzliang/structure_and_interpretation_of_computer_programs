(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(require 'build_class_using_lambda)
(require 'represent_tables_with_mutable_data)
(require 'stream_delayed_lists)

(ert-deftest one-count-test ()
  ;; These three should test only once without re-evaluate the source code
  ;; because every call (one-count) will add 1
  (should (equal (one-count) 1))
  (should (equal (one-count) 2))
  (should (equal (one-count) 3))
  )

(ert-deftest make-count-test ()
  (defalias 'count1 (make-count))
  (defalias 'count2 (make-count))

  (should (equal (count1) 1))
  (should (equal (count1) 2))
  (should (equal (count1) 3))
  (should (equal (count2) 1))
  (should (equal (count2) 2))
  (should (equal (count1) 4))
  (should (equal (count1) 5))
  )

(ert-deftest global-and-local-count-test ()
  (defalias 'global-and-local-count1 (make-count-global-and-local))
  (defalias 'global-and-local-count2 (make-count-global-and-local))

  ;; Each counter has its own local count value and also the global count value
  (should (equal (global-and-local-count1) '(1 1)))
  (should (equal (global-and-local-count1) '(2 2)))
  (should (equal (global-and-local-count2) '(1 3)))
  (should (equal (global-and-local-count1) '(3 4)))
  (should (equal (global-and-local-count2) '(2 5)))
  )

(ert-deftest global-and-local-with-message-passing-count-test ()
  (defalias 'global-and-local-with-message-passing-count1 (make-count-global-and-local-with-message-passing))
  (defalias 'global-and-local-with-message-passing-count2 (make-count-global-and-local-with-message-passing))

  (should (equal (funcall (global-and-local-with-message-passing-count1 'local)) 1))
  (should (equal (funcall (global-and-local-with-message-passing-count1 'local)) 2))
  (should (equal (funcall (global-and-local-with-message-passing-count1 'global)) 1))
  (should (equal (funcall (global-and-local-with-message-passing-count1 'global)) 2))
  (should (equal (funcall (global-and-local-with-message-passing-count2 'local)) 1))
  (should (equal (funcall (global-and-local-with-message-passing-count2 'global)) 3))

  ;; set local value of count2 to 8
  (should (equal (funcall (global-and-local-with-message-passing-count2 'set-local-value) 8) "okay"))
  (should (equal (funcall (global-and-local-with-message-passing-count2 'local)) 9))
  )

(ert-deftest make-buzzer-test ()
  (defalias 'buzzer (make-buzzer))
  (should (equal (funcall (buzzer 'local)) 1))
  (should (equal (funcall (buzzer 'local)) 2))
  (should (equal (funcall (buzzer 'set-local-value) 5) "okay"))
  (should (equal (funcall (buzzer 'local)) 6))
  (should (equal (funcall (buzzer 'local)) "buzz"))
  (should (equal (funcall (buzzer 'set-local-value) 13) "okay"))
  (should (equal (funcall (buzzer 'local)) "buzz"))
  )

(ert-deftest represent-tables-with-mutable-data-test ()
  (should (equal the-table '(*table*)))
  (should (equal (table-put-value 'first-key 'first-value) 'ok))
  (should (equal (table-put-value 'second-key 'second-value) 'ok))
  (should (equal (table-put-value 'third-key 'third-value) 'ok))
  (should (equal (table-get-value 'first-key) 'first-value))
  (should (equal (table-get-value 'third-key) 'third-value))
  )

(ert-deftest my-stream-test ()
  (should (equal (my-stream-show stream-ones) '(1 1 1 1 1 1 1 1 1 1 ...)))
  (should (equal (my-stream-show stream-integers) '(1 2 3 4 5 6 7 8 9 10 ...)))
  (should (equal (my-stream-show stream-even-integers) '(2 4 6 8 10 12 14 16 18 20 ...)))
  (should (equal (my-stream-show stream-primes) '(2 3 5 7 11 13 17 19 23 29 ...)))
  (setq max-specpdl-size 1400)				;; Limit on number of Lisp variable bindings and unwind-protects. Default 1300 is too small for 20 primes' calculation	
  (should (equal (my-stream-show stream-primes 20) '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 ...)))
  )
