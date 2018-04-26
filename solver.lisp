(load "packages.lisp")
(in-package :ctdnslvr)

(defconstant +set-length+ 9)
(defconstant +word-list-file+ "oxford.txt")

(defmacro subseq-or-end (sequence start end)
  "Returns subsequence between start and end unless end is larger than the sequence. Then just returns the whole sequence"
  (let ((ending-value-name (gensym)))
    `(let ((,ending-value-name ,sequence))
       (subseq ,ending-value-name ,start (if (< (length ,ending-value-name) ,end)
					     (length ,ending-value-name)
					     ,end)))))

(defun find-match (word set)
  "Check whether word matches the set. Returns the word if it is otherwise returns nil.
Assume set is sorted."
  (let ((start 0))
    (sort word #'char<)
    (dotimes (x (length word))
      (unless (setf start (position (elt word x) set :start start))
	(return-from find-match))
      (incf start))
    t))

(defun search-word-list (word-list set)
  "Takes in the word set and searches for words that match. Returns a list of matching words sorted with longest first"
  (let ((result ()))
    (dotimes (i (length word-list))
      (if (find-match (first (elt word-list i)) set)
	  (push (second (elt word-list i)) result)))
    (sort result (lambda (x y) (> (length x) (length y))))))

(defun get-letter-set ()
  "Get letters from user, remove all the whitespace and make sure there are only 9"
  (format *query-io* "Input letters: ")
  (force-output *query-io*)
  (let ((set (read-line *query-io*)))
    (loop
       (setf set (remove #\space set))
       (cond ((< (length set) +set-length+)
	      (format *query-io* "Not enough letters (~a/~a). Add more: " (length set) +set-length+)
	      (setf set (concatenate 'string set (read-line *query-io*))))
	     ((> (length set) +set-length+)
	      (return (subseq set 0 +set-length+)))
	     (t (return set))))))  

(defun load-words-from-file ()
  "Returns a list of pairs (sorted word , word)"
  (let ((word-list ()))
    (with-open-file (wordfile +word-list-file+)
      (loop for line = (read-line wordfile nil)
	    while line do (push (list
				 (sort (copy-seq line) #'char<)
				 line)
				word-list)))
    (reverse word-list)))

(defun start ()
  (let ((word-list (load-words-from-file)))
    (format t "Word list length: ~a~%" (length word-list))
    (loop
       (format t "~2&~{~10a~}" (subseq-or-end (search-word-list word-list (sort (get-letter-set) #'char<)) 0 10))
       (unless (y-or-n-p "~%Again: ")
	 (return))
       (format t "~%"))))
