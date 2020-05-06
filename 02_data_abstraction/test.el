(require 'ert)

(require 'implement_data_pair_using_procedure_only)
(require 'tree_hierachical_data)
(require 'map_deep_list)
(require 'generic_operators)

(ert-deftest my-pair-test ()
  (should (equal (my-car (my-cons 3 4)) 3))
  (should (equal (my-cdr (my-cons 3 4)) 4))
  (should-error (funcall (my-cons 3 4) 'bad-input))
  )

(ert-deftest my-tree-test ()
  (let ((my-tree
         (make-tree 1
                    (list (make-tree 2 (make-leaves 3 4))
                          (make-tree 5 (make-leaves 6 7 8))
                          )
                    )
         ))
    (should (equal my-tree '(1 (2 (3) (4)) (5 (6) (7) (8)))))
    (defun square (x) (* x x))
    ;; apply square function to every item of my-tree
    (should (equal (tree-map 'square my-tree) '(1 (4 (9) (16)) (25 (36) (49) (64)))))
    ;; depth-first-search
    (should (equal (with-output-to-string (depth-first-search my-tree))
                   "1 2 3 4 5 6 7 8 "))
    ;; breadth-first-search
    (should (equal (with-output-to-string (breadth-first-search my-tree))
                   "1 2 5 3 4 6 7 8 "))
    )
  )

(ert-deftest my-deep-list-map-test ()
  (defun square (x) (* x x))
  (should (equal (deep-list-map 'square '((2 3) (3 4 5) (6 (7 (8)))))
                 '((4 9) (9 16 25) (36 (49 (64))))))
  )

(ert-deftest my-generic-operators-test ()
  (defvar square-5 (make-square 5))
  (defvar circle-5 (make-circle 5))

  (should (equal (area-conventional square-5) 25))
  (should (equal (area-conventional circle-5) 78.53981633974483))
  (should (equal (perimeter-conventional square-5) 20))
  (should (equal (perimeter-conventional circle-5) 31.41592653589793))
  (should-error (area-conventional triangle-5))

  (should (equal (area-data-directed square-5) 25))
  (should (equal (area-data-directed circle-5) 78.53981633974483))
  (should (equal (perimeter-data-directed square-5) 20))
  (should (equal (perimeter-data-directed circle-5) 31.41592653589793))
  (should-error (area-data-directed triangle-5))

  (defvar square-5-message-passing (make-square-message-passing 5))
  (defvar circle-5-message-passing (make-circle-message-passing 5))

  (should (equal (area-message-passing square-5-message-passing) 25))
  (should (equal (area-message-passing circle-5-message-passing) 78.53981633974483))
  (should (equal (perimeter-message-passing square-5-message-passing) 20))
  (should (equal (perimeter-message-passing circle-5-message-passing) 31.41592653589793))
  (should-error (area-message-passing triangle-5))
  )
