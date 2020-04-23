;; Higher order procedures generalize a pattern
;; Generalizing patterns is how we keep our program from getting really long
;; Programming is easy as long as we can see the whole program all at once

(defun my-sum (fn-as-data a b)
  (if (> a b)
      0
    (+ (funcall fn-as-data a) (my-sum fn-as-data (+ a 1) b))
    )
  )

(defun square (x) (* x x))

(defun cube (x) (* x x x))


(defun my-choose (fn-as-data my-list)
  (cond ((null my-list) '())
        ((funcall fn-as-data (first my-list))
         (cons (first my-list) (my-choose fn-as-data (cdr my-list))))
        (t (my-choose fn-as-data (cdr my-list)))
        )
  )


(provide 'function_as_data)
