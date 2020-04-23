(require 'ert)
(require 'function_as_data)

(ert-deftest function-as-data-test ()
  ;; 3*3 + 4*4 + 5*5 = 50
  (should (equal (my-sum 'square 3 5) 50))
  )
