;; We are going to use generate operator to calculate perimeter and area for different shapes
;; |--------+------------+------------|
;; |        | perimeter  | area       |
;; |--------+------------+------------|
;; | square | s-->4s     | s-->s*s    |
;; |--------+------------+------------|
;; | circle | r-->2*pi*r | r-->pi*r*r |
;; |--------+------------+------------|

;; Type tagging
;; Tagging data allows data with the same pattern to represent different type of objects
;; For example, 5 cannot represent square or circle, because we cannot distinguish it from number 5
;; ('square 5) ('circle 5) can represent a square whose side length is 5 or a circle whose radius is 5

(defalias 'attach-tag 'cons)
(defalias 'get-tag 'car)
(defalias 'get-contents 'cdr)

(defun make-square (side-length)
  (attach-tag 'square side-length)
  )

(defun make-circle (radius)
  (attach-tag 'circle radius)
  )

;; There are three ways to do generic operations: conventional way, data-directed programming, message passing programming

;; conventional way
(defun area-conventional (shape)
  (cond ((equal (get-tag shape) 'square)
         (* (get-contents shape) (get-contents shape)))
        ((equal (get-tag shape) 'circle)
         (* pi (get-contents shape) (get-contents shape)))
        (t (error "Unknown shape"))
        )
  )

(defun perimeter-conventional (shape)
  (cond ((equal (get-tag shape) 'square)
         (* 4 (get-contents shape)))
        ((equal (get-tag shape) 'circle)
         (* 2 pi (get-contents shape)))
        (t (error "Unknown shape"))
        )
  )

;; The convetional way of dealing with generic operations has only one problem
;; In order to introduce a new feature (add a new shape), we have to modify and add code to previous code that is already working
;; this brings the possibility of adding bugs and breaking code that used to work
;; So our goal is, whenever possible, never have to go back and change working code in order to add new features


;; data-directed programming
;; in data-directed programming, we put all functions to calculate perimeter and area to a table
;; then we look up the table to decide which function to use to calculate
;; Preferences in software are files of data to tell one generic software what operations should be used
(put 'square 'area (lambda (s) (* s s)))
(put 'circle 'area (lambda (r) (* pi r r)))
(put 'square 'perimeter (lambda (s) (* 4 s)))
(put 'circle 'perimeter (lambda (r) (* 2 pi r)))
;; This way, in order to add a new shape, we just need to put another functions as data to the table
;; no existing code needs to be changed

(defun operate-data-directed (operation obj)
  (let ((procedure-to-operate (get (get-tag obj) operation)))
    (if procedure-to-operate
        (funcall procedure-to-operate (get-contents obj))
      (error "Unknown operator for type")
      )
    )
  )
;; syntactic sugar
(defun area-data-directed (shape)
  (operate-data-directed 'area shape)
  )
(defun perimeter-data-directed (shape)
  (operate-data-directed 'perimeter shape)
  )


;; Message passing programming makes adding object type very easy
;; and existing objects dosen't need to be changed
;; message passing programming is the heart of object oriented programming
(defun make-square-message-passing (side-length)
  `(lambda (message)
    (cond ((equal message 'area)
           (* ,side-length ,side-length))
          ((equal message 'perimeter)
           (* 4 ,side-length))
          (t (error "Unknown message"))
          )
    )
  )

(defun make-circle-message-passing (radius)
  `(lambda (message)
    (cond ((equal message 'area)
           (* pi ,radius ,radius))
          ((equal message 'perimeter)
           (* 2 pi ,radius))
          (t (error "Unknown message"))
          )
    )
  )

(defun operate-message-passing (operation obj)
  ;; Now obj is a function
  (funcall obj operation)
  )

;; syntactic sugar
(defun area-message-passing (shape)
  (operate-message-passing 'area shape)
  )
(defun perimeter-message-passing (shape)
  (operate-message-passing 'perimeter shape)
  )

;; conventional way makes it easy to add operators, hard to add objects
;; message passing make it easy to add objects, hard to add operators


(provide 'generic_operators)
