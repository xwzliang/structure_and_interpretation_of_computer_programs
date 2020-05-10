;; The car of the table is a sentinel node or dummy node
;; other nodes of the table contains key-value pair

(defvar the-table)
(setq the-table (list '*table*))

(defun table-get-value (key)
  (let ((record (assoc key (cdr the-table))))
    (if record
        (cdr record)		;; return the value associcated with the key
      nil
      )
    )
  )

(defun table-put-value (key value)
  (let ((record (assoc key (cdr the-table))))
    (if (not record)
        ;; put the key value pair to the head of the data
        (setcdr the-table
                (cons (cons key value)
                      (cdr the-table)))
      (setcdr record value)
      )
    )
  'ok
  )

(provide 'represent_tables_with_mutable_data)
