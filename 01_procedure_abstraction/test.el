(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(require 'function_as_data)

(ert-deftest my-sum-test ()
  ;; 3*3 + 4*4 + 5*5 = 50
  (should (equal (my-sum 'square 3 5) 50))
  ;; 3*3*3 + 4*4*4 + 5*5*5 = 216
  (should (equal (my-sum 'cube 3 5) 216))
  )

(ert-deftest my-choose-test ()
  ;; Choose even number from list
  (should (equal (my-choose 'evenp '(1 3 4 5 8 9))
                 '(4 8)))
  ;; Choose string that contains letter e
  (should (equal (my-choose (lambda (str) (s-contains-p "e" str))
                            '("gotta" "get" "you" "in" "my" "life"))
                 '("get" "life")))
  )

(ert-deftest make-adder-fn-test ()
  (should (equal (plus3 8) 11))
  (should (equal (plus5 8) 13))
  (should (equal (plus5 (plus3 8)) 16))
  (should (equal (funcall (make-adder-fn 9) 8) 17))
  )

(ert-deftest compose-fns-test ()
  (should (equal (second-in-list
                  '("gotta" "get" "you" "in" "my" "life"))
                 "get"))
  ;; (square (square 3)) = 81
  (should (equal (funcall (fn-apply-twice 'square) 3) 81))
  )
