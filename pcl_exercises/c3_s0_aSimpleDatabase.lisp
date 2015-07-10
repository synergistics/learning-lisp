;;; Chapter 3. A Simple Database

(defvar *db* nil)

(defun make-cd (title artist rating)
  (list :title title :artist artist :rating rating))

(defun add-record (cd)
  (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-cd ()
  (make-cd 
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)))

(defun add-cd-session ()
  (add-record (prompt-cd))
  (if (y-or-n-p "Add another CD?") (add-cd-session)))

(defun save-db (filename) 
  (with-open-file (output-stream filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* output-stream))))

(defun load-db (filename)
  (with-open-file (input-stream filename)
    (with-standard-io-syntax 
      (setf *db* (read input-stream)))))

(defun select (predicate)
  (remove-if-not predicate *db*))

(defun where (&rest args)
  (setf keys (loop for (key value) on args by #'cddr collect key))
  (lambda (cd)
     (every (lambda (key) (equal (getf args key) (getf cd key))) keys)))
