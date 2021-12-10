(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defun get-row (line) (coerce line 'list))

(defun get-cell (grid x y)
  (cond
    ((< x 0)                      #\9) ; nth can't handle negative indices.
    ((< y 0)                      #\9) ; so we pretend there's a border of 9s around the grid.
    ((null (nth x (nth y grid)))  #\9)
    (t     (nth x (nth y grid)))))

(defun north (grid x y)
  (get-cell grid x (- y 1)))

(defun south (grid x y)
  (get-cell grid x (+ y 1)))

(defun east (grid x y)
  (get-cell grid (+ x 1) y))

(defun west (grid x y)
  (get-cell grid (- x 1) y))

(defun all (pred xs)
  (cond
    ((null xs) t)
    ((funcall pred (car xs)) (all pred (cdr xs)))
    (t nil)))

(defun is-greater-than (cell)
  (lambda (x) (char< cell x)))

(defun id (x) x)

(defun filter (pred xs &optional rest)
  (cond
    ((null xs)
      (nreverse rest))
    ((funcall pred (car xs))
      (filter pred (cdr xs) (cons (car xs) rest)))
    (t
      (filter pred (cdr xs) rest))))

(defun is-low (grid x y)
  (all #'id
    (mapcar (is-greater-than (get-cell grid x y))
      (list (north grid x y) (east grid x y) (south grid x y) (west grid x y)))))

; https://tailrecursion.com/jlt/posts/collecting.html
(defun collector ()
  "Maintains the head of a list and returns a function that appends an
item to the list. When the returned function is called with no
arguments, returns the head."
  (let ((tail nil)
        (head nil))
    (lambda (&optional (item nil argument?))
      (cond
        ((not argument?) head)
        ((null tail) (setq tail (cons item nil)
                           head tail))
        (t (let ((new-tail (cons item nil)))
             (setf (cdr tail) new-tail
                   tail new-tail)))))))

(print
  (let (
    (tubes  (mapcar #'get-row (read-file-as-lines "09-input.txt")))
    (collect (collector))
  )
    (let (
      (coords
        (dotimes (y (length tubes) (funcall collect))
          (dotimes (x (length (car tubes)) (funcall collect))
            (funcall collect (list x y)))))
    )
      (let (
        (lows (mapcar #'digit-char-p
                (mapcar (lambda (coord) (get-cell tubes (car coord) (cadr coord)))
                  (filter
                    (lambda (coord) (is-low tubes (car coord) (cadr coord)))
                    coords))))
      )
        (+ (reduce #'+ lows) (length lows))))))