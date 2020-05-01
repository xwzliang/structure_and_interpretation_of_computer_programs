;; Making alias for these functions provides a level of abstraction
;; it not only makes the code more readable, but also more maintainable
;; when we want change the way to make-tree, we can just change the definition for it
;; instead of having to change the implementation function cons, which may be buried in some other cons functions
;; those cons functions may not be used as to make-tree at all (this makes the change much much harder)
(defalias 'make-tree 'cons)
(defalias 'get-datum 'car)
(defalias 'get-children 'cdr)

(defun make-leaves (&rest sequence)
  (mapcar (lambda (x)
            (make-tree x '()))
          sequence)
  )

;; map function to every item of tree, without changing the tree structure
;; Mutual Recursion: tree-map calls mapcar, mapcar calls tree-map
(defun tree-map (fn-to-map tree)
  (make-tree (funcall fn-to-map (get-datum tree))
             (mapcar (lambda (child) (tree-map fn-to-map child))
                     (get-children tree))
             )
  )

(defun depth-first-search (tree)
  (prin1 (get-datum tree))
  (princ " ")
  (mapcar 'depth-first-search (get-children tree))
  )

(defun breadth-first-search (tree)
  (bfs-iter (list tree))
  )

(defun bfs-iter (queue)
  ;; queue is just a list
  (if (not (null queue))
      (let ((queue-head (car queue)))
        (prin1 (get-datum queue-head))
        (princ " ")
        ;; enqueue queue-head's children
        (bfs-iter (append (cdr queue) (get-children queue-head)))
        )
    )
  )

(provide 'tree_hierachical_data)
