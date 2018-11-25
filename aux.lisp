	;;;
;;; Some auxiliary functions
;;;

(defpackage :aux 
  (:use :common-lisp)
  (:export :multiset-table
		   :uniq
		   :tsv-to-list
		   :strip-package-deco
		   :starts-with-p
		   :empty-string-p
		   :read-from-file
		   :string-to-list
		   :csv-to-str-list
		   :shuffle-list
		   :read-file-as-string 
		   ))

(in-package aux)


(defmacro flip-bool (b)
  `(setf ,b (not ,b)))



(defun multiset-table (&optional (ht (make-hash-table)))
  "a closure based hash table implementation of alist
   to put, call with key and val -- duplicates are allowed;
   to get, call with just key
   call with :check key to check existence
   call with :count to get count"
  #'(lambda (&rest input)
	  (let ((head (car input))
			(tail (cadr input)))
		(cond ((equal head ':count) (hash-table-count ht))
			  ((equal head ':check) 
			   (nth-value 1 (gethash tail ht)))
			  ((equal head ':get-table) ht)
			  (t 
				(if tail
				  (if (nth-value 1 (gethash head ht))
					(setf (gethash head ht) (cons tail (gethash head ht)))
					(setf (gethash head ht) (list tail)))
				  (if (nth-value 1 (gethash head ht))
					(nth-value 0 (gethash head ht))
					(error "Key unknown."))))))))

(defun uniq (lst &optional (comparator #'equal))
  "like Unix uniq"
  (if (endp lst)
	nil
	(cons (car lst)
		   (uniq (remove-if 
				   #'(lambda (x)
					   (funcall comparator (car lst) x))
				   (cdr lst))
				 comparator))))

(defun tsv-to-list (path)
  "convert a tab-separated value file to a list"
  (labels ((text-to-list (text)
						 (read-from-string 
						   (concatenate 'string "(" text ")")))
		   (read-lines (str &optional acc)
					   (let ((line (read-line str nil 'eof)))
						 (if (equal line 'eof)
						   acc
						   (read-lines 
							 str 
							 (append
							   acc
							   (list (text-to-list line))))))))
	(with-open-file (input-stream 
					  (if (typep path 'string)
						(make-pathname :name path)
						path)
					  :direction :input)
	  (read-lines input-stream))))

(defun strip-package-deco (expr)
  (read-from-string
	(format nil "~A" expr)))

(defun starts-with-p (str prefix)
  "check whether the str starts with the prefix -- both are strings"
  (and 
	(>= (length str) (length prefix))
	(string-equal prefix (subseq str 0 (length prefix)))))

(defun empty-string-p (str) 
  (and 
	(stringp str)
	(zerop (length str))))

(defun read-from-file (pathname)
  "return a list of lisp objects in the file at the pathname object"
  (with-open-file (str pathname :direction :input)
	(do ((x (read str nil :eof) (read str nil :eof))
		 (store nil (cons x store)))
	  ((eq x :eof) (reverse store)))))

(defun string-to-list (str)
  " \"a b c\" => (a b c)"
  (read-from-string (concatenate 'string "(" str ")")))

(defun csv-to-str-list (pname &optional (field-marker #\,) (text-delimiter #\"))  
  "read a csv file into a list of string (cell) lists"
  (labels ((proc-line (line)
					  (let ((store nil)
							(cell-buffer (make-string 1000))
							(pos 0)
							(protect nil)) ; double quote delimited protect zone is initially inactive
						(do ((index 0 (+ index 1)))
						  ((= index (length line))
						   (reverse
							 (cons (subseq cell-buffer 0 pos) store)))
						  (let ((current-char (char line index)))
							(cond ((eq current-char text-delimiter) (flip-bool protect))
								  (protect 
									(setf (char cell-buffer pos) current-char)
									(incf pos))
								  ((eq current-char field-marker)
								   (push (subseq cell-buffer 0 pos) store)
								   (setf pos 0))
								  (t
									(setf (char cell-buffer pos) current-char)
									(incf pos)))))))) 
	  (with-open-file (str pname :direction :input)
		(do ((x (read-line str nil :eof) (read-line str nil :eof))
			 (store nil (cons (proc-line x) store)))
		  ((eq x :eof) (reverse store))))))


(defun read-file-as-string (pname)
  (with-open-file (str pname :direction :input) 
	(do ((line (read-line str nil :eof) (read-line str nil :eof))
		 (store "" (concatenate 'string store (format nil "~A~%" line))))
	  ((eq line :eof) store))))





(defun random-pick (seq)
  "randomly pick an element from a sequence"
  (elt seq (random (length seq))))

(defun shuffle-list (lst &optional shuffled)
  (labels ((remove-first (item lst)
						 (if (equal (car lst) item)
						   (cdr lst)
						   (cons (car lst) (remove-first item (cdr lst))))))
	
  (if (endp lst)
	shuffled
	(let ((pick (random-pick lst)))
	  (shuffle-list (remove-first pick lst) (cons pick shuffled))))))
