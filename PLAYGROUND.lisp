(defun f (x)
  (when (< (g x) 3)
    (h x 2)))

(format t (f 3))

