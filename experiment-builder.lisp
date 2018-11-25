;;;
;;; Builds jspsych experiments
;;;

(load "aux.lisp")

(setf *random-state* (make-random-state t))

(defparameter *timeline* nil)

(defparameter *item-file* (make-pathname :name "resources/Items.csv"))

(defparameter *item-repo* (aux:csv-to-str-list *item-file*))

(defparameter *trial-count* (let ((init -1)) #'(lambda () (incf init))))

(defparameter *groups* '((bil "bil") (zannet "zannet") (dusun "düşün")))

(defparameter *header*  
"<!DOCTYPE html>
<html>
 <meta charset=\"UTF-8\"> 
	<head>
		<title>Experiment</title>
		<script src=\"jspsych.js\"></script>
		<script src=\"plugins/jspsych-instructions.js\"></script>
		<script src=\"plugins/custom-survey-likert.js\"></script>
		<script src=\"plugins/custom-survey-multi-choice.js\"></script>
		<script src=\"plugins/jspsych-html-button-response.js\"></script>
		<link href=\"css/jspsych.css\" rel=\"stylesheet\" type=\"text/css\"></link>
	</head>
	<body></body>
	<script>")

(defparameter *scale* "['Üzülür','','','','Sevinir']")

(defun build-likert (id texts &optional (scale *scale*))
  (declare (ignore id))
  (concatenate 'string 
	"var trial_" (write-to-string (funcall *trial-count*)) "= {
		type: 'survey-likert',
	    preamble: '<div style=\"margin: 50px auto; width: 1000px; height: 250px; background-color: rgb(220, 220, 220)\"><br/><br/>"
		(car texts)
		"<br/><br/><br/>"
		(cadr texts)
		"</div>',
		questions: [{prompt: '', labels:"
		scale
		", required: true}],
  }"
  ))

(defun build-timeline ()
  (format nil "[~{trial_~a,~}]" (let ((store))
								  (dotimes (x (funcall *trial-count*)(reverse store))
									(push x store)))))

(defun build-footer ()
  (concatenate 'string
  "
  jsPsych.init({
    timeline:" (build-timeline) ",
    on_finish: function() {
      jsPsych.data.displayData();
    },
    default_iti: 250
  });
	</script>
</html>"))

(defmacro update-string (name str)
  `(setf ,name (concatenate 'string ,name ,str)))

(defun build-experiment (group)
  (let ((exp-path (make-pathname :name (concatenate 'string (string (car group)) "-exp.html")))
		(store *header*))
	(update-string store (build-likert 8 '("a" "b")))
	(update-string store (build-footer))
	(with-open-file (str exp-path :direction :output :if-does-not-exist :create :if-exists :overwrite)
	  (format str "~A" store))))

