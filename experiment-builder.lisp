;;;
;;; Builds jspsych experiments
;;;

(load "aux.lisp")
(setf *random-state* (make-random-state t))

(defparameter *timeline* nil)
(defparameter *filler-profile* '(3 2 3 3 2 2 3))
(defparameter *header-path*  (make-pathname :name "resources/header"))
(defparameter *header*  (aux:read-file-as-string *header-path*))
(defparameter *preamble-path*  (make-pathname :name "resources/preamble"))
(defparameter *preamble*  (aux:read-file-as-string *preamble-path*))
(defparameter *item-file* (make-pathname :name "resources/Items.csv"))
(defparameter *item-repo* (aux:csv-to-str-list *item-file*))
(defparameter *trial-count* (let ((init -1)) #'(lambda () (incf init))))
(defparameter *groups* '((bil "bili") (zannet "zannedi") (dusun "düşünü")))

(defparameter *scale* "['Üzülür','','','','Sevinir']")




(defun insert-crit (text)
  (position #\$ text))


(defun build-likert (item &key (replace-dollar nil) (scale *scale*))
  (let ((id (first item))
		(text1 (second item))
		(text2 (third item))
		(question (fourth item)))
	(concatenate 'string 
	"
	var trial_" (write-to-string (funcall *trial-count*)) "= {
		type: 'survey-likert',
	  	id: '"
		id
		"',
	    preamble: '<div style=\"margin: 50px auto; width: 1000px; height: 250px; background-color: rgb(220, 220, 220)\"><br/><br/>"
		text1
		" "
		(if replace-dollar
		  (aux:replace-char-with-str #\$ replace-dollar text2)
		  text2)
		"<br/><br/><br/>"
		question
		"</div>',
		questions: [{prompt: '', labels:"
		scale
		", required: true}],
  };
  ")))

(defun build-question (item)
  (let* ((id (car item))
		 (prompt (cadr item))
		 (options (cddr item))
		 (correct (car options))
		 (new-options (aux:shuffle-list options)))
	(concatenate 'string
	"
	var trial_" (write-to-string (funcall *trial-count*)) "= {
      type: 'survey-multi-choice',
	  id: '"
	  id
      "',
	  correct: '"
	  correct
	  "',
      questions: [{prompt: \"" prompt  "\", options: ['" (first new-options) "', '" (second new-options) "', '" (third new-options) "'], required: true}],
  };
  ")))

(defun declare-group (group)
  (concatenate 'string
			   "
	// set the group 
	var group_id = \"" (string-downcase (string group)) "\";"))


(defun build-timeline ()
	(format nil "[~{trial_~a,~}]" (let ((store))
									(dotimes (x (funcall *trial-count*) (reverse store))
									  (push x store)))))

(defun build-footer ()
  (concatenate 'string
  "
  jsPsych.init({
    timeline:" (build-timeline) ",
	on_finish: function(){ saveData(fname, jsPsych.data.get().csv()); },
    default_iti: 250
  });
	</script>
</html>"))

(defmacro update-string (name str)
  `(setf ,name (concatenate 'string ,name ,str)))


(defun fetch-items (char-prefix)
  " c: critical f: filler; they come paired with their questions" 
  (mapcar 
	#'(lambda (x)
		(let* ((id (car x))
			   (num (subseq id 1 3))
			   (key (concatenate 'string "Q" num)))
		  (list x (assoc key *item-repo* :test #'equal))))
	(remove-if-not
	  #'(lambda (x)
		  (let ((word (car x)))
			(and (plusp (length word)) (eq (char (car x) 0) char-prefix))))
	  *item-repo*)))

(defun make-item-repo (char-prefix)
  (let ((item-list (aux:shuffle-list (fetch-items char-prefix))))
	#'(lambda ()
		(pop item-list))))

(defparameter *give-critical* (make-item-repo #\C))
(defparameter *give-filler* (make-item-repo #\F))



(defun build-experiment (group)
  (let ((exp-path (make-pathname :name (concatenate 'string (string (car group)) "-exp.html")))
		(store *header*))
	(update-string store (declare-group (car group)))
	(update-string store *preamble*)
	(dolist (n *filler-profile*)
	  (dotimes (i n)
		(let* ((item (funcall *give-filler*))
			   (filler (car item))
			   (question (cadr item)))
		  (update-string store (build-likert filler))
		  (update-string store (build-question question))))
	  (let* ((item (funcall *give-critical*))
			 (critical (car item))
			 (question (cadr item)))
		(unless (null item)
		  (update-string store (build-likert critical :replace-dollar (cadr group)))
		  (update-string store (build-question question)))))
	(update-string store (build-footer))
	(with-open-file (str exp-path :direction :output :if-does-not-exist :create :if-exists :overwrite)
	  (format str "~A" store))))

(defun main ()
  (dolist (g *groups*)
	(print g)
	(build-experiment g)
	(setf *trial-count* (let ((init -1)) #'(lambda () (incf init))))
	(setf *give-critical* (make-item-repo #\C))
	(setf *give-filler* (make-item-repo #\F))
	
	))
