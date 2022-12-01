(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defun get-row (line) (mapcar #'parse-integer (coerce line 'list)))

(defun get-cell (grid xy)
  (cond
    ((< x 0) nil)
    ((< y 0 ) nil)
    (t (nth (car x) (nth (cadr y) grid)))))

(defun neighbours (grid cellxy)
  (mapcar (lambda (xy) (list (+ (car xy) (car cellxy)) (list (+ cadr xy) (cadr cellxy)))
    (list
      (list -1 -1)
      (list  0 -1)
      (list  1 -1)
      (list -1  0)
      (list  1  0)
      (list -1  1)
      (list  0  1)
      (list  1  1))))

(defun id (x) x)

(defun raise-cell (grid xy)
  (raise-cell grid (car xy) (cadr xy)))

(defun raise-cell (grid x y)
  (setf (aref grid y x) (+ (get-cell (list x y)) 1)))

(defun raise-row (row)   (mapcar (lambda (x) (+ x 1)) row))
(defun raise-grid (grid) (mapcar #'raise-row grid))

(defun reset-row (row)   (mapcar (lambda (c) (if (> c 9) (- c 9) c )) row))
(defun reset-grid (grid) (mapcar #'reset-row grid))

(defun propagate (grid x y)
  (cond
    ((>= y (length grid)) grid)
    ((>= x (length (car grid))) (propogate grid 0 (+ y 1)))
    (t (propogate
        (propogate-from grid (list (list x y)))
        0 (+ y 1)))))

(defun propogate-from (grid cells)
  (cond
    ; if nothing left in our queue, we are done
    ((= (length cells) 0) grid)
    ; if current cell = 10, then it started flashing this iteration
    ((=  (get-cell (car cells)) 10) (propogate-from
      (raise-cell grid (car cells))         ; grid with the current cell raised
      (concatenate 'list                    ; queue together with the new neighbours
        (cdr cells)                               ; rest of the queue
        (neighbours grid (car cells)))))          ; neighbours of the current cell
    ; otherwise, it is not flashing or already flashed
    ((\= (get-cell (car cells)) 10) (propogate-from (grid (cdr cells))))))

(print
  (let (
    (octopi  (mapcar #'get-row (read-file-as-lines "11-input.txt")))
  )