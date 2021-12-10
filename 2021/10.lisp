(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defun get-row (line) (coerce line 'list))

(defparameter *opens* (list #\( #\{ #\[ #\< ))
(defparameter *closes* (list #\) #\} #\] #\> ))
(defparameter *points* (list 3 1197 57 25137 ))

(defun index-of (x xs)
  (index-of-i x xs 0))

(defun index-of-i (x xs i)
  (cond ((null xs) nil)
        ((char= (car xs) x) i)
        (t (index-of-i x (cdr xs) (+ i 1)))))

(defun pair (closing)
  (nth (index-of closing *closes*) *opens*))

(defun score-of (closing)
  (if (null closing)
    0
    (nth (index-of closing *closes*) *points*)))

(defun check-line (line &optional stack)
  (let ((next (car line))
        (rest (cdr line)))
    (cond
      ((null line) nil)
      ((member next *opens*)  (check-line rest (cons next stack)))
      ((member next *closes*)
        (if (char= (pair next) (car stack))
          (check-line rest (cdr stack))
          next)))))

(defun filter (pred xs &optional rest)
  (cond
    ((null xs)
      (nreverse rest))
    ((funcall pred (car xs))
      (filter pred (cdr xs) (cons (car xs) rest)))
    (t
      (filter pred (cdr xs) rest))))

(defun not-null (x) (not (null x)))

(print
  (let ((lines  (mapcar #'get-row (read-file-as-lines "10-input.txt"))))
    (reduce #'+
      (mapcar #'score-of
          (mapcar #'check-line lines)))))
