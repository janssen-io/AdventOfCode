(defparameter *xmin*   70)
(defparameter *xmax*   96)
(defparameter *xmas* "🎅")
(defparameter *ymin* -179)
(defparameter *ymax* -124)

(defmacro between (left op1 middle op2 right)
  `(and
    (,op1 ,left ,middle )
    (,op2 ,middle ,right)))

(defvar highest 0)
(defvar num_sol 0)

(defun hitp (px py)
  (and
    (between *xmin* <= px <= *xmax*)
    (between *ymin* <= py <= *ymax*)))

(defun overshotp (px py)
  (or
    (> px *xmax*)
    (< py *ymin* )))

(defun simulate (px py vx vy ymax)
  (cond
    ((hitp px py) ymax)
    ((overshotp px py) nil)
    (t (simulate
        (+ px vx)
        (+ py vy)
        ; vx for this input is bigger than 0,
        ; so it always decreases to 0
        (max 0 (- vx 1))
        (- vy 1)
        (max py ymax)))))

(defvar ys
  (remove-if #'null
    (loop for x from 0 to (+ 1 *xmax*)
      append (loop for y from *ymin* to 1000
        collect (simulate 0 0 x y 0)))))

(print (loop for y in ys maximizing y))
(print (length ys))
(print *xmas*)