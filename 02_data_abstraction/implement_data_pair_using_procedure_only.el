;; (my-car (my-cons 3 4)) = 3
;; my-cons returns a procedure
;; my-car and my-cdr apply the procedure with argument
;; thus we could implement cons, car, and cdr without using any data structures at all, but only using procedures. And if we access pairs using only cons, car, and cdr we cannot distinguish this implementation from one that uses "real" data structures.

(defun my-cons (x y)
  `(lambda (which-part)
    (cond ((equal which-part 'my-car) ,x)
          ((equal which-part 'my-cdr) ,y)
          (t (error "Bad argument input for cons") which-part)
     )
    )
  )

(defun my-car (pair)
  (funcall pair 'my-car)
  )

(defun my-cdr (pair)
  (funcall pair 'my-cdr)
  )

(provide 'implement_data_pair_using_procedure_only)
