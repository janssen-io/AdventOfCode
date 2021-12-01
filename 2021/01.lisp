(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defun get-measurements (filename)
  (mapcar #'parse-integer
    (remove-if (lambda (s) (string-equal s ""))
      (read-file-as-lines filename))))

(defun a (depths)
  (if (= (length depths) 1)
    0
    (destructuring-bind (x y &rest _) depths
      (+ (if (< x y) 1 0) (a (cdr depths))))))

(defun b (depths)
  (if (= (length depths) 3)
    0
    (destructuring-bind (x _ _ y &rest _) depths
      (+ (if (< x y) 1 0) (b (cdr depths))))))

(defun c (windowSize)
  "Create a calculator with specific window size"
  (defun cl (depths)
    (if (= (length depths) windowSize)
      0
      (let ((x (car depths)) (y (nth windowSize depths)))
        (+ (if (< x y) 1 0) (cl (cdr depths)))))))

(print
  (a
    (get-measurements "01-input.txt")))

(print
  (b
    (get-measurements "01-input.txt")))

(print
  (funcall (c 3)
    (get-measurements "01-input.txt")))

(terpri)

