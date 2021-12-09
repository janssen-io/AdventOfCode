(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defun string-split (delim chars &optional parts acc)
  (cond
    ((= (length chars) 0)
      (reverse (cons (reverse acc) parts)))
    ((< (length chars) (length delim))
      (string-split delim (subseq chars 1) parts (concatenate 'string (subseq chars 0 1) acc)))
    (t
      (let
        ((substring (subseq chars 0 (length delim)))
        (tail (subseq chars 1)))
        (cond
          ((string= substring delim)
            (string-split delim (subseq chars (length delim)) (cons (reverse acc) parts)))
          (t
            (string-split delim tail parts (concatenate 'string (subseq chars 0 1) acc))))))))

(defun select (pred xs &optional selected)
  (cond
    ((null xs) selected)
    ((null selected)                  (select pred (cdr xs) (car xs)))
    ((funcall pred (car xs) selected) (select pred (cdr xs) (car xs)))
    (t                                (select pred (cdr xs) selected))))

(defun parse-numbers (line)
  (string-split " " line))

(defun parse-notes (line)
  (mapcar #'parse-numbers       ; [0] = [0-9], [1] = [output]
    (string-split " | " line))) ; [0] = 0-9,   [1] = output

(defun get-output (notes)
  (cadr notes))

(defun filter (pred xs &optional rest)
  (cond
    ((null xs)
      (nreverse rest))
    ((funcall pred (car xs))
      (filter pred (cdr xs) (cons (car xs) rest)))
    (t
      (filter pred (cdr xs) rest))))

(defun is-known (x)
  (or
    (= (length x) 2)
    (= (length x) 3)
    (= (length x) 4)
    (= (length x) 7)))

(defun filter-known (x)
  (filter #'is-known x))

(print
  (let (
    (notes (mapcar #'parse-notes (read-file-as-lines "08-input.txt")))
  )
    (reduce #'+
      (mapcar #'length
        (mapcar #'filter-known
          (mapcar #'get-output notes))))))