(require 'ert)

(require 'implement_data_pair_using_procedure_only)

(ert-deftest my-pair-test ()
  (should (equal (my-car (my-cons 3 4)) 3))
  (should (equal (my-cdr (my-cons 3 4)) 4))
  (should-error (funcall (my-cons 3 4) 'bad-input))
  )
