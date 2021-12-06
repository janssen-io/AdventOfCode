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

(print
  (sum
    (simulate 256
      (count-fishes (record)
        (parse-fishes
          (read-file-as-lines "06-input.txt"))))))