(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defun get-commands (filename)
  "Returns a list of lists (direction number)"
  (mapcar #'read-from-string
    (remove-if (lambda (s) (string-equal s ""))
      (read-file-as-lines filename))))

(defun a (commands)
  (defun a-acc (cs acc)
    (if (= (length cs) 0)
      acc
      (destructuring-bind (head &rest tail) cs
        (let (
            (dir (car head))
            (amount (cadr head))
            (hor (car acc))
            (dep (cadr acc)))
          (cond
            ((string= "forward" dir) (a-acc tail (list (+ hor amount) dep)))
            ((string= "down" dir)    (a-acc tail (list hor (+ dep amount))))
            (t                       (a-acc tail (list hor (- dep amount)))))))))
  (a-acc commands (list 0 0)))

(print
  (reduce #'*
    (a
      (get-commands "02-input.lisp.txt"))))


(terpri)

