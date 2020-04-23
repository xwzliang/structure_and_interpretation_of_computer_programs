(defun my-sum (fn-as-data a b)
  (if (> a b)
	  0
	(+ (funcall fn-as-data a) (my-sum fn-as-data (+ a 1) b))
	)
  )

(defun square (x) (* x x))

(provide 'function_as_data)
