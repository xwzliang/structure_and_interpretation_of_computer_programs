;; deep list (abitrary list of list ...) can be seen as a tree like structure
;; some of the nodes have children but no datum
;; other nodes have datum but no children

;; When you see two or more recursive calls will happen in sequence
;; you are looking at a tree recusion
;; There is either implicitly or explicitly a tree like data structure involved.

(defun deep-list-map (fn-to-map list-of-list)
  (if (listp list-of-list)
      ;; nodes that have children but no datum, map call deep-list-map recusively for every children
      (mapcar (lambda (element)
                (deep-list-map fn-to-map element))
              list-of-list)
    ;; other nodes that have datum but no children, apply fn-to-map to the datum
    (funcall fn-to-map list-of-list)
    )
  )

(provide 'map_deep_list)
