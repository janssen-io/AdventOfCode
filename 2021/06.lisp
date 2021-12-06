(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defun psh (xs x)
  (nconc xs (list x)))

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

(defun record ()
  (list 0 0 0 0 0 0 0 0 0))

(defun count-fishes (fs lifetimes)
  (if (null lifetimes)
    fs
    (count-fishes
      (progn
        (setf (nth (car lifetimes) fs) (+ (nth (car lifetimes) fs) 1))
        fs)
      (cdr lifetimes))))

(defun parse-fishes (lines)
  (mapcar #'parse-integer
    (string-split "," (car lines))))

(defun cycle (fs)
  (let (
    (babies (car fs))
    (rest (cdr fs)))
    (progn
      (setf (nth 6 rest) (+ (nth 6 rest) babies)))
      (psh rest babies)))

(defun simulate (days fs)
  (if (= days 0)
    fs
    (simulate (- days 1) (cycle fs))))

(defun sum (xs)
  (reduce #'+ xs))

(defun replace-if (predicate xs replacement &optional acc)
  (cond
    ((null (car xs))               (reverse acc))
    ((funcall predicate (car xs)) (replace-if predicate (cdr xs) replacement (cons replacement acc)))
    (t                            (replace-if predicate (cdr xs) replacement (cons (car xs) acc)))))

(defun cycle-naive (fishes)
  (let (
    (next-fishes (mapcar (lambda (fish) (- fish 1)) fishes))
    (numBabies (count-if (lambda (f) (= f 0)) fishes))
  )
  (concatenate 'list
    (replace-if (lambda (f) (= f -1)) next-fishes 6) ; old-fishes
    (make-list numBabies :initial-element 8)))) ; new fishes

(defun simulate-naive (days fishes)
  (if (= days 0)
    fishes
    (simulate-naive (- days 1) (cycle-naive fishes))))

; (defun simulate-any (algorithm days &rest args)
;   (if (= days 0)
;     args
;     (simulate (- days 1) (apply algorithm args))))

(print
  (sum
    (simulate 256
      (count-fishes (record)
        (parse-fishes
          (read-file-as-lines "06-input.txt"))))))

(print
  (length
    (simulate-naive 80
        (parse-fishes
          (read-file-as-lines "06-example.txt")))))