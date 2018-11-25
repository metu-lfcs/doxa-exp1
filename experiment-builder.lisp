;;;
;;; Builds jspsych experiments
;;;

(load "aux.lisp")
(setf *random-state* (make-random-state t))

(defparameter *timeline* nil)
(defparameter *header-path*  (make-pathname :name "resources/header"))
(defparameter *header*  (aux:read-file-as-string *header-path*))
(defparameter *item-file* (make-pathname :name "resources/Items.csv"))
(defparameter *item-repo* (aux:csv-to-str-list *item-file*))
(defparameter *trial-count* (let ((init -1)) #'(lambda () (incf init))))
(defparameter *groups* '((bil "bili") (zannet "zannedi") (dusun "düşünü")))

(defparameter *scale* "['Üzülür','','','','Sevinir']")

(defun build-likert (id text question &optional (scale *scale*))
  (concatenate 'string 
	"var trial_" (write-to-string (funcall *trial-count*)) "= {
		type: 'survey-likert',
	  	id: '"
		(string id)
		"',
	    preamble: '<div style=\"margin: 50px auto; width: 1000px; height: 250px; background-color: rgb(220, 220, 220)\"><br/><br/>"
		text
		"<br/><br/><br/>"
		question
		"</div>',
		questions: [{prompt: '', labels:"
		scale
		", required: true}],
  }
  "
  ))

(defun build-question (id prompt options)
  (concatenate 'string
	"var trial_" (write-to-string (funcall *trial-count*)) "= {
      type: 'survey-multi-choice',
	  id: '"
	  (string id)
      "',
      questions: [{prompt: \"" prompt  "\", options: ['" (first options) "', '" (second options) "', '" (third options) "'], required: true}],
  }
  "))


(defun build-timeline ()
  (format nil "[~{trial_~a,~}]" (let ((store))
								  (dotimes (x (funcall *trial-count*)(reverse store))
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

(defun build-experiment (group)
  (let ((exp-path (make-pathname :name (concatenate 'string (string (car group)) "-exp.html")))
		(store *header*))
	(update-string store (build-likert 'c01 "a" "b"))
	(update-string store (build-question 'q01 "Why?" '("one" "two" "three")))
	(update-string store (build-footer))

	(with-open-file (str exp-path :direction :output :if-does-not-exist :create :if-exists :overwrite)
	  (format str "~A" store))))

