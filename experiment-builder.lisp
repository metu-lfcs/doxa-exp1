;;;
;;; Builds jspsych experiments
;;;
    
(load "aux.lisp")
(setf *random-state* (make-random-state t))

(defmacro update-string (name str)
  `(setf ,name (concatenate 'string ,name ,str)))

(defparameter *header*
  (aux:read-file-as-string (make-pathname :name "resources/header")))
(defparameter *preamble*
  (aux:read-file-as-string (make-pathname :name "resources/preamble")))
(defparameter *consent-block*
  (aux:read-file-as-string (make-pathname :name "resources/consent-block")))
(defparameter *footer*
  (aux:read-file-as-string (make-pathname :name "resources/footer")))
(defparameter *item-repo*
  (aux:csv-to-str-list (make-pathname :name "resources/Items.csv")))

(defparameter *trial-store* "")

(defparameter *critical-items* nil)
(defparameter *filler-items* nil)
(defparameter *question-items* nil)
(defparameter *arith-items* nil)

(defparameter *critical-repo* "")
(defparameter *filler-repo* "")
(defparameter *question-repo* "")
(defparameter *arith-repo* "")

(defparameter *trial-count* (let ((init -1)) #'(lambda () (incf init))))

(defparameter *choices* "['Üzülür','Bilemiyorum','Hisleri değişmez','Sevinir']")

(defun build-item (item &key (replace-dollar nil))
  (let ((id (first item))
        (text1 (second item))
        (text2 (third item))
        (question (fourth item)))
    (concatenate 'string 
    "
    var trial_" id "= {
        type: 'survey-multi-choice',
          id: '"
        id
        "',
        preamble: '<div style=\"margin: 50px auto; width: 800px; height: 250px; background-color: rgb(220, 220, 220)\"><br/><br/>"
        text1
        "<br/>"
        (if replace-dollar
          (aux:replace-char-with-str #\$ replace-dollar text2)
          text2)
        "<br/><br/><br/>"
        question
        "</div>'.replace(\"$\",dollar_repl),
        questions: [{prompt: '', options: jsPsych.randomization.shuffle(" *choices* "), required: true}],
  };
  ")))


(defun build-arith (item)
  (let* ((id (car item))
         (prompt (concatenate 'string (second item) " " (third item) " " (fourth item) "\\'" (fifth item)))
         (options (subseq item 5 7))
         (correct (car options))
         (new-options (aux:shuffle-list options)))
    (concatenate 'string
    "
    var trial_" id "= {
      type: 'survey-multi-choice',
      id: '"
      id
      "',
      correct: '"
      correct
      "',
      preamble: '<div style=\"margin: 50px auto; width: 800px; height: 250px; background-color: rgb(220, 220, 220)\"><br/><br/><br/><br/>"
      prompt
      "</div>',
      questions: [{prompt: '', options: ['" (first new-options) "', '" (second new-options) "'], required: true}],
  };
  ")))

(defun build-question (item)
  (let* ((id (first item))
         (prompt (second item))
         (options (subseq item 2 5))
         (correct (car options))
         (new-options (aux:shuffle-list options)))
    (concatenate 'string
    "
    var trial_" id "= {
      type: 'survey-multi-choice',
      id: '"
      id
      "',
      correct: '"
      correct
      "',
      preamble: '<div style=\"margin: 50px auto; width: 800px; height: 250px; background-color: rgb(220, 220, 220)\"><br/><br/><br/><br/>"
      prompt
      "</div>',
      questions: [{prompt: '', options: ['" (first new-options) "', '" (second new-options) "', '" (third new-options) "'], required: true}],
  };
  ")))


(defun build-timeline ()
    (concatenate 'string
    "
    var timeline = [welcome,instructions];

    var filler_profile = [3,2,3,3,2,2,3];

    var filler_repo = jsPsych.randomization.shuffle([" *filler-repo* "]); 

    var critical_repo = jsPsych.randomization.shuffle([" *critical-repo* "]);

    var arith_repo = jsPsych.randomization.shuffle([" *arith-repo* "]);

    var filler_clock = 0;
    var critical_clock = 0;
    var arith_clock = 0;

    for (var i = 0; i < filler_profile.length - 1; i++){
        var k = filler_profile[i];
        for (var j = 0; j < k; j++){
            timeline = timeline.concat(filler_repo[filler_clock]);
            timeline.push(arith_repo[arith_clock])
            arith_clock++;
            filler_clock++;
        }
        timeline = timeline.concat(critical_repo[critical_clock]);
        timeline.push(arith_repo[arith_clock]);
        arith_clock++;
        critical_clock++;
    }

    for (var i = 0; i < filler_profile[-1]; i++){
        timeline = timeline.concat(filler_repo[filler_clock]);
        timeline.push(arith_repo[arith_clock])
        arith_clock++;
        filler_clock++;
    }

    timeline.push(goodbye);


  "))

(defun build-footer ()
  (concatenate 'string
  "
  jsPsych.init({
    timeline:" (build-timeline) ",
    default_iti: 250
  });
    </script>
</html>"))


(defun build-trials () 
  (dolist (i *item-repo*)
    (let ((head (car i)))
      (unless (zerop (length (car i)))
        (case (char head 0)
          (#\C
           (update-string *trial-store* (build-item i))
           (update-string *critical-repo*
                          (format nil "[trial_~A,trial_Q~A]," head (subseq head 1 3))))
          (#\F
           (update-string *trial-store* (build-item i))
           (update-string *filler-repo*
                          (format nil "[trial_~A,trial_Q~A]," head (subseq head 1 3))))
          (#\A 
           (update-string *trial-store* (build-arith i))
           (update-string *arith-repo*
                          (format nil "trial_~A," head)))
          (#\Q 
           (update-string  *trial-store* (build-question i))))))))



(defun build-experiment ()
  (let ((exp-path (make-pathname :name "main.html"))
        (store *header*))
    (if (probe-file exp-path)
      (delete-file exp-path))
    (build-trials)
    (update-string store *preamble*)
    (update-string store *consent-block*)
    (update-string store *trial-store*)
    (update-string store (build-timeline))
    (update-string store *footer*)
    (with-open-file (str exp-path :direction :output :if-does-not-exist :create :if-exists :overwrite)
      (format str "~A" store))))

(build-experiment)
