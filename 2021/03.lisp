(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defun get-bytes (filename)
    (remove-if (lambda (s) (string-equal s ""))
      (read-file-as-lines filename)))

(defun minc (n)
  "Increment a number or return 1 if nil"
  (if (null n)
    1
    (+ n 1)))

(defun tally (table row bit)
  "Increment the (0 1) records for a given 'significance' (row)"
  (let (
    (a (car (nth row table)))
    (b (cadr (nth row table))))
  (if (string= bit "1")
    (setf (nth row table) (list (minc a) b))
    (setf (nth row table) (list a (minc b)))
  )
  table))

(defun parseByte (table bits pos)
  "Increment the 0 & 1 counts for a given byte"
  (if (= (length bits) pos)
    table
    (parseByte (tally table pos (char bits pos)) bits (+ pos 1))))

(defun a (bytes)
  "Calculate the number of 0s and 1s in each bit"
  (defun a-acc (bs table)
    (if (= (length bs) 0)
      table
      (a-acc (cdr bs) (parseByte table (car bs) 0))))
  (a-acc
    bytes
    (make-list (length (car bytes)) :initial-element (list 0 0) )))

(defun fst (row) (car row))
(defun snd (row) (cadr row))

(defun to-bin (table res)
  "Create a binary number from the tallies"
  (if (= (length table) 0)
    res
    (let (
      (row (car table))
    )
      (if (> (fst row) (snd row))
        (concatenate 'string "1" (to-bin (cdr table) res))
        (concatenate 'string "0" (to-bin (cdr table) res))))))

(defun maxByte (byte) (- (expt 2 (+ (length byte) 0)) 1))
(defun b2d (byte) (parse-integer byte :radix 2))

(format t "~a"
  (let (
    (most
      (to-bin
        (a
          (get-bytes "03-input.txt"))
        ""))
  )
    (*
      (b2d most)
      (logxor (maxByte most) (b2d most)))))

(terpri)
