;; abstraction is a process of enabling you to think in terms of the problem you are trying to solve, rather than thinking in terms of the limitations of computer hardware.
;; abstraction is very complicated to implement, but very easy to use.


;; Dynamic and Lexical Binding

;; dynamic
;; All variable names and their values live in one global table. A name that is dynamically bound is looked up only in bindings in the dynamic environment of the name – that is, in all bindings which have been created since the program began and which have not yet been destroyed. When there are multiple bindings in the dynamic environment, the most recently created one is used.
;; lexical
;; Each binding scope (function, let syntax, …) creates a new table of variable names and values, organised in a hierarchy called “the environment”. A name that is lexically bound is looked up only in bindings in the lexical environment of the name – that is, in bindings that enclose the name in the source code.

;; If lexical binding is needed for whole file without using lexical-let, put following line to the first line of a file
;;; -*- lexical-binding: t -*-

(require 'cl)

(defalias 'one-count
  (lexical-let ((count-value 0))
    (lambda ()
      (setq count-value (+ count-value 1))
      count-value)
    )
  )
;; (let ((x 0)) exp-body) is just a syntactic sugar for ((lambda (x) exp-body) 0)
;; a lambda function and then call it with argument

;; if count-value is not lexically bound, the counter won't work
;; because when dynamically bount, the variable count-value has been destroyed by the time lambda is applied

;; A factory to make many counters
(defun make-count ()
  (lexical-let ((count-value 0))
    (lambda ()
      (setq count-value (+ count-value 1))
      count-value)
    )
  )


;; local state means object has its own memory

;; Actually we are making a class object here which has global class variables and local instance variables
;; using only primitives
(defalias 'make-count-global-and-local
  (lexical-let ((global-value 0))
    (lambda ()
      (lexical-let ((local-value 0))
        (lambda ()
          (setq local-value (+ local-value 1))
          (setq global-value (+ global-value 1))
          (list local-value global-value)
          )
        )
      )
    )
  )


;; A class object with message passing
(defalias 'make-count-global-and-local-with-message-passing
  (lexical-let ((global-value 0))
    (lambda ()
      (lexical-let ((local-value 0))
        (lambda (message)   ;; dispatch procedure that takes a message, and decide which procedure to return
          ;; An object is represented by its dispatch procedure
          (cond ((equal message 'local)
                 (lambda ()
                   (setq local-value (+ local-value 1))
                   local-value))
                ((equal message 'global)
                 (lambda ()
                   (setq global-value (+ global-value 1))
                   global-value))
                ((equal message 'set-local-value)
                 (lambda (new-value)
                   (setq local-value new-value)
                   (message "okay")))
                (t (error "No such method" message))
                )
          )
        )
      )
    )
  )


;; Class with inheritance

;; Inheritance is what makes object-oriented programming practical
;; we don't have to copy and paste procedure all over the place

;; a buzzer returns buzz if the number is dividable by 7
(defalias 'make-buzzer
  (lambda ()
    (lexical-let ((parent-count (make-count-global-and-local-with-message-passing)))
      (lambda (message)
        (cond ((equal message 'local)
               (lambda ()
                 (let ((count-value (funcall (funcall parent-count 'local))))
                   (if (equal (% count-value 7) 0)
                       (message "buzz")
                       count-value)
                   )
                 )
               )
              ;; other message just passed to parent to handle
              (t (funcall parent-count message))
              )
        )
      )
    )
  )

(provide 'build_class_using_lambda)
