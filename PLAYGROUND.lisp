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
(defun find-ordered-index (elements predicate index &optional (key index))
  (cond 
    ((or (= index 0) (= key 0)) 0)
    ((funcall predicate (nth (- key 1) elements) (nth index elements)) (find-ordered-index elements predicate index (- key 1))) 
    (t key)))

;;;; Slicing lists and Insertion Sort

;;; Remember that every time insertion sort is called, a recursive function will run multiple times, ruining performance, hence, slice is now iterative
;;; This slice function exists in CL as the subseq function
(defun slice (elements from-index to-index) 
  (loop
    for i from from-index to (- to-index 1)
    collect (nth i elements)))

(defun slice-2 (elements from-index to-index &optional (accumulator '()))
  (cond 
    ((= from-index to-index) accumulator)
    (t (slice-2 elements (+ from-index 1) to-index (cons accumulator (append (nth from-index elements) nil))))))

(defun splice (elements from-index to-index)
  (append (slice elements 0 from-index) (slice elements to-index (length elements))))

;;; Check hyperspec for copy-list and rotatef to replace this function
(defun move (elements from-index to-index) 
  (let ((spliced (splice elements from-index (+ from-index 1))))
    (append (slice spliced 0 to-index) (list (nth from-index elements)) (slice spliced to-index (length spliced)))))

;;; Can be implented with CL core
(defun find-ordered-index (elements predicate index &optional (key index))
  (cond 
    ((or (= index 0) (= key 0)) 0)
    ((funcall predicate (nth (- key 1) elements) (nth index elements)) (find-ordered-index elements predicate index (- key 1))) 
    (t key)))

(defun insertion-sort (elements predicate &optional (index 0))
  (cond
    ((= index (length elements)) elements)
    (t (insertion-sort (move elements index (find-ordered-index elements predicate index)) predicate (+ index 1)))))

(defun generate-random-list ()
  (loop
    for i from 1 to 1000 collect (random 100)))
