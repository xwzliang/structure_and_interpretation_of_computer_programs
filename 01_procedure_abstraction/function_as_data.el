;; Higher order procedures generalize a pattern
;; Generalizing patterns is how we keep our program from getting really long
;; Programming is easy as long as we can see the whole program all at once

;; lisp has a uniform for both procedure and data, (a b c) can be a procedure or a list
;; this makes lisp very good at handling its own programs

;; First Class Data Type
;; Can be:
;; - the value of a variable
;; - an argument to a procedure
;; - the value returned by a procedure
;; - a member of an aggregate
;; - anonymous

(require 'cl-lib)
(require 'cl)

;; procudure that takes a function as argument
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


;; procedure that return a function
(defun make-adder-fn (num)
  (lexical-let ((num num))
    (lambda (x) (+ x num))
    )
  )

(fset 'plus3 (make-adder-fn 3))

(fset 'plus5 (make-adder-fn 5))


;; procedure that takes functions as arguments and return a function

;; why do we need to handle fn-f, fn-g specially?
;; because when we define a function in another function, normally the variables of inner one are protected from outer one
;; so in order to use the variable value in outer function, we need to tell lisp to evaluate those first

;; (defun compose-fns (fn-f fn-g)
;;   (lexical-let ((fn-f fn-f)
;;                 (fn-g fn-g))
;;     (lambda (x) (funcall fn-f (funcall fn-g x)))
;;     )
;;   )
(defun compose-fns (fn-f fn-g)
    `(lambda (x) (,fn-f (,fn-g x)))
  )

(fset 'second-in-list (compose-fns 'car 'cdr))

(defun fn-apply-twice (fn)
  (compose-fns fn fn)
  )

(provide 'function_as_data)
