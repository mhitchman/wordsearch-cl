(defconstant +set-length+ 9)
(defconstant +word-list-file+ "oxford.txt")

;; (defun find-subsets (word-list-file set)
;;   "Read lines from word-list-file and compare to set to find words that are a subset"
;;   (let ((result ()))
;;     (loop for line = (read-line word-list-file nil)
;;        while line do (if (subsetp line set)
;; 			 (push line result)))
;;     result))

(defun search-word-file (set)
  (let ((result ()))
    (with-open-file (wordList +word-list-file+)
      (loop for line = (read-line wordList nil)
	 while line do (if (find-match line set)
			   (push line result))))
    (sort result (lambda (x y) (> (length x) (length y))))))

(defun find-match (word set)
  "Check whether word matches the set. Returns the word if it is otherwise returns nil.
Assume set is sorted."
  (let ((sorted-word (sort (copy-seq word) #'char<)) (start 0))
    (dotimes (x (length sorted-word))
      (unless (setf start (position (elt sorted-word x) set :start start))
	(return-from find-match))
      (incf start))
    word))

(defun get-letter-set ()
  "Get letters from user, remove all the whitespace and make sure there are only 9"
  (format *query-io* "Input letters: ")
  (force-output *query-io*)
  (let ((set (read-line *query-io*)))
    (loop
       (remove #\space set)
       (cond ((< (length set) +set-length+)
	      (format *query-io* "Not enough letters (~a/~a). Add more: " (length set) +set-length+)
	      (setf set (concatenate 'string set (read-line *query-io*))))
	     ((> (length set) +set-length+)
	      (return (subseq set 0 +set-length+)))
	     (t (return set))))))  


(defun start ()
  (search-word-file (sort (get-letter-set ) #'char<)))
