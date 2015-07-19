;;; 2.21 (GISP)
(defun coupler (arg1 arg2 arg3 arg4) 
 `((,arg1 ,arg2) (,arg3 ,arg4)))

;;; 2.22 (GISP)
(defun duo-cons (head1 head2 tail)
  (cons head1 (cons head2 tail)))

;;; Traverse a list recursively (example)
(defun traverse (list)
  (if (not (equal (length list) 0))
    (traverse (cdr list))
    (print list)))

;;; 4.10 (GISP)
(defun constrain (x low high)
  (cond ((> x high) high)
        ((< x low) low)
        (t x)))

;;; 4.12 (GISP) (modified)
;;; NOTE: Implement Y-combinator to allow anonymous recursion
(defun make-cycle (direction bound)
  )

;;; 4.22 (GISP)
(defun boilingp (temp scale)
  (if (>= temp (getf scale :temp)) t))

;;; Implementing nth
(defun implemented-nth (n list) 
  (if (= n 0) (car list) (implemented-nth (- n 1) (cdr list))))

;;; 4.29 (GISP)
(defun logical-and-1 (x y) 
  (cond (x (cond (y t)))))

;;; Slicing lists
(defun slice (elements from-index to-index) 
  (cond
    ((= from-index to-index) nil)
    (t (cons (nth from-index elements) (slice elements (+ from-index 1) to-index)))))

(defun slice-2 (elements from-index to-index &optional (accumulator '()))
  (cond 
    ((= from-index to-index) accumulator)
    (t (slice-2 elements (+ from-index 1) to-index (cons accumulator (append (nth from-index elements) nil))))))

(defun splice (elements from-index to-index)
  (append (slice elements 0 from-index) (slice elements to-index (length elements))))

(defun move (elements from-index to-index) 
  (let ((spliced (splice elements from-index (+ from-index 1))))
    (append (slice spliced 0 to-index) (list (nth from-index elements)) (slice spliced to-index (length spliced)))))

(defun insertion-sort (elements &optional (index 1))
  (cond
    ((= index (length elements))) elements)
    (t )))
