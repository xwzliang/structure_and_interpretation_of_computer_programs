;; Stream is very important, it is a separation of the form of a program from the actual general sequence of events that it generates

(setq the-empty-stream nil)

(defun my-stream-empty-p (stream)
  (eq stream the-empty-stream)
  )

(defun my-delay (expressions)
  `(lambda () ,expressions)
  )

(defun my-force (expressions)
  (funcall expressions)
  )

(defmacro my-stream-cons (car-of-stream cdr-of-stream)
  (list 'cons car-of-stream (my-delay cdr-of-stream))
  )

(defalias 'my-stream-car 'car)

(defun my-stream-cdr (stream)
  (my-force (cdr stream))
  )

(defun my-stream-map (fn stream)
  (lexical-let ((fn fn)
                (stream stream))
    (if (my-stream-empty-p stream)
        the-empty-stream
      (my-stream-cons (funcall fn (my-stream-car stream))
                      (my-stream-map fn (my-stream-cdr stream))
                      )
      )
    )
  )

(defun my-stream-filter (fn stream)
  (lexical-let ((fn fn)
                (stream stream))
    (if (my-stream-empty-p stream)
        the-empty-stream
      (if (funcall fn (my-stream-car stream))
          (my-stream-cons (my-stream-car stream) (my-stream-filter fn (my-stream-cdr stream)))
        (my-stream-filter fn (my-stream-cdr stream))
        )
      )
    )
  )

(defun my-stream-show (stream &optional item-num)
  (unless item-num (setq item-num 10))
  (if (> item-num 0)
      (cons (my-stream-car stream) (my-stream-show (my-stream-cdr stream) (1- item-num)))
    '(...)
    )
  )

;; A stream of infinite ones
(defvar stream-ones (my-stream-cons 1 stream-ones))

;; Integer stream
(defvar stream-integers
  (my-stream-cons 1 (my-stream-map '1+ stream-integers))
  )

;; even integers
(defvar stream-even-integers (my-stream-filter 'evenp stream-integers))

;; prime numbers
(defun divisible-p (x y) (= (% x y) 0))

(defun sieve-of-Eratosthenes-find-primes (stream)
  (lexical-let ((stream stream))
    (my-stream-cons
     (my-stream-car stream)
     (sieve-of-Eratosthenes-find-primes (my-stream-filter
                                         (lambda (x) (not (divisible-p x (my-stream-car stream))))
                                         (my-stream-cdr stream)
                                         )
                                        )
     )
    )
  )

(defvar stream-primes (sieve-of-Eratosthenes-find-primes (my-stream-cdr stream-integers)))


(provide 'stream_delayed_lists)
