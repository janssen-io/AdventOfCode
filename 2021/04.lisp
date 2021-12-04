(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defun chars-to-string (chars)
  (coerce chars 'string))

(defun split (delim str)
  (mapcar #'chars-to-string
    (string-split
      delim
      (coerce str 'list))))

(defun string-split (delim chars &optional parts acc)
  (let ((c (car chars)) (tail (cdr chars)))
    (cond
      ((null c)        (reverse (cons (reverse acc) parts)))
      ((char= c delim) (string-split delim tail
                        (cons (reverse acc) parts)))
      (t               (string-split delim tail parts
                        (cons c acc))))))

(defun get-numbers (lines)
  (mapcar #'parse-integer
    (split #\, (car lines))))

(defun get-row (line)
  (remove-if (lambda (x) (or (string= "" x) (null x)))
    (split #\Space line)))

(defun parse-row (line)
  (mapcar #'parse-integer
    (get-row line)))

(defun parse-boards (lines &optional boards board)
  (let ((line (car lines)) (tail (cdr lines)))
    (cond
     ((null line)       (reverse           (cons (reverse board) boards)))
     ((string= line "") (parse-boards tail (cons (reverse board) boards) nil))
     (t                 (parse-boards tail boards (cons (parse-row line) board))))))

; (print (get-numbers  (read-file-as-lines "04-input.txt")))
; (terpri)

(print (parse-boards (cddr (read-file-as-lines "04-input.txt"))))
(terpri)
