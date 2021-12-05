(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defparameter *grid*
  (make-hash-table))

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

(defun get-row (line)
  (mapcar
    (lambda (coords) (mapcar #'parse-integer coords))
    (mapcar
      (lambda (line) (string-split "," line))
      (string-split " -> " line))))

(defun x1 (segment) (caar segment))
(defun x2 (segment) (caadr segment))
(defun y1 (segment) (cadar segment))
(defun y2 (segment) (cadadr segment))

(defun swap (segment)
  (list
    (list (x2 segment) (y2 segment))
    (list (x1 segment) (y1 segment))))

(defun correct-segment (segment)
  (cond
    ( (> (x1 segment) (x2 segment)) (swap segment))
    ( (> (y1 segment) (y2 segment)) (swap segment))
    ( t                             segment)))

; used for part 1
(defun is-straight (segment)
  (or
    (= (y1 segment) (y2 segment))
    (= (x1 segment) (x2 segment))))

(defun inc (x)
  (if (null x) 1 (+ x 1)))

(defun k (cell)
  (+
    (car cell)
    (* (cadr cell) 10000)))

(defun inc-grid (cell)
  (setf (gethash (k cell) *grid*) (inc (gethash (k cell) *grid*)))
  *grid*)

; this works because the diagonal lines are always at 45 degrees
; the filling helps creating points for straight segments
(defun zip-fill (xs ys)
  "Zip until one list is exhausted, then continue zipping using the last element"
  (let (
    (x (car xs))
    (y (car ys))
  )
    (cond
      ((and (> (length xs) 1) (> (length ys) 1))
        (cons (list x y) (zip-fill (cdr xs) (cdr ys))))
      ((and (> (length xs) 1) (= (length ys) 1))
        (cons (list x y) (zip-fill (cdr xs) ys)))
      ((and (= (length xs) 1) (> (length ys) 1))
        (cons (list x y) (zip-fill xs (cdr ys))))
      ((and (= (length xs) 1) (= (length ys) 1))
        (list (list x y)))
      (t (list))
  )))

(defun range (low high)
  (if (> low high)
    (reverse (loop for n from high to low collect n))
    (loop for n from low to high collect n)))

(defun draw-segment (segment)
  (let (
    (xs (range (x1 segment) (x2 segment)))
    (ys (range (y1 segment) (y2 segment))))

    (mapcar (lambda (coord) (inc-grid coord)) (zip-fill xs ys))
    *grid*))

(defun count-intersections ()
  (length
    (remove-if
      (lambda (x) (< x 2))
      (loop for v being each hash-value of *grid* collect v))))

(mapcar
  #'draw-segment
  ; (remove-if-not #'is-straight ; uncomment for part 1
    (mapcar #'correct-segment
      (mapcar #'get-row
        (read-file-as-lines "05-input.txt"))))

(print
  (count-intersections))

