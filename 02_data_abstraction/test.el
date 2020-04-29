(require 'ert)

(require 'implement_data_pair_using_procedure_only)
(require 'tree_hierachical_data)

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
    )
  )
