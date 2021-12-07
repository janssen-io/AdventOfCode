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

(defun range (low high)
  (if (> low high)
    (reverse (loop for n from high to low collect n))
    (loop for n from low to high collect n)))

(defun minimum (xs)
  (select #'< xs))

(defun maximum (xs)
  (select #'> xs))

(defun select (pred xs &optional selected)
  (cond
    ((null xs) selected)
    ((null selected)                  (select pred (cdr xs) (car xs)))
    ((funcall pred (car xs) selected) (select pred (cdr xs) (car xs)))
    (t                                (select pred (cdr xs) selected))))

(defun parse-crabs (lines)
  (mapcar #'parse-integer
    (string-split "," (car lines))))


(defun fuel (transform crabs position)
  (reduce #'+
    (mapcar
      (lambda (crab) (funcall transform (abs (- crab position))))
      crabs)))

(defun id (x) x)

(print
  (let (
    (crabs (parse-crabs (read-file-as-lines "07-input.txt")))
  )
  (minimum
    (mapcar
      (lambda (position) (fuel #'id crabs position))
      (range (minimum crabs) (maximum crabs))))))

(defun gauss (n) (/ (* n (+ n 1)) 2))

(print
  (let (
    (crabs (parse-crabs (read-file-as-lines "07-input.txt")))
  )
  (minimum
    (mapcar
      (lambda (position) (fuel #'gauss crabs position))
      (range (minimum crabs) (maximum crabs))))))