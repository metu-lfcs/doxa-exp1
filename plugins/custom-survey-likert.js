/**
 * custom-survey-likert
 * adapted from
 *
 * jspsych-survey-likert
 * a jspsych plugin for measuring items on a likert scale
 *
 * Josh de Leeuw
 *
 * documentation: docs.jspsych.org
 *
 */

jsPsych.plugins['survey-likert'] = (function() {

  var plugin = {};

  plugin.info = {
    name: 'survey-likert',
    description: '',
    parameters: {
      questions: {
        type: jsPsych.plugins.parameterType.COMPLEX,
        array: true,
        pretty_name: 'Questions',
        nested: {
          prompt: {type: jsPsych.plugins.parameterType.STRING,
                     pretty_name: 'Prompt',
                     default: undefined,
                     description: 'Questions that are associated with the slider.'},
          labels: {type: jsPsych.plugins.parameterType.STRING,
                   array: true,
                   pretty_name: 'Labels',
                   default: undefined,
                   description: 'Labels to display for individual question.'},
          required: {type: jsPsych.plugins.parameterType.BOOL,
                     pretty_name: 'Required',
                     default: false,
                     description: 'Makes answering questions required.'}
        }
      },
      preamble: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Preamble',
        default: null,
        description: 'String to display at top of the page.'
      },
      button_label: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Button label',
        default:  'Continue',
        description: 'Label of the button.'
      }
    }
  }

  plugin.trial = function(display_element, trial) {


    var html = "";
    // inject CSS for trial
    html += '<style id="custom-survey-likert-css">';
    html += ".custom-survey-likert-statement { display:block; font-size: 16px; padding-top: 40px; margin-bottom:10px; }"+
      ".custom-survey-likert-opts { list-style:none; width:100%; margin:0; padding:0 0 35px; display:block; font-size: 14px; line-height:1.1em; }"+
      ".custom-survey-likert-opt-label { line-height: 1.1em; color: #444; }"+
      ".custom-survey-likert-opts:before { content: ''; position:relative; top:11px; /*left:9.5%;*/ display:block; background-color:#efefef; height:4px; width:100%; }"+
      ".custom-survey-likert-opts:last-of-type { border-bottom: 0; }"+
      ".custom-survey-likert-opts li { display:inline-block; /*width:19%;*/ text-align:center; vertical-align: top; }"+
      ".custom-survey-likert-opts li input[type=radio] { display:block; position:relative; top:0; left:50%; margin-left:-6px; }"
    html += '</style>';

    // show preamble text
    if(trial.preamble !== null){
      html += '<div id="custom-survey-likert-preamble" class="custom-survey-likert-preamble">'+trial.preamble+'</div>';
    }
    html += '<form id="custom-survey-likert-form">';

    // add likert scale questions
    for (var i = 0; i < trial.questions.length; i++) {
      // add question
      html += '<label class="custom-survey-likert-statement">' + trial.questions[i].prompt + '</label>';
      // add options
      var width = 100 / trial.questions[i].labels.length;
      var options_string = '<ul class="custom-survey-likert-opts" data-radio-group="Q' + i + '">';
      for (var j = 0; j < trial.questions[i].labels.length; j++) {
        options_string += '<li style="width:' + width + '%"><input type="radio" name="Q' + i + '" value="' + j + '"';
        if(trial.questions[i].required){
          options_string += ' required';
        }
        options_string += '><label class="custom-survey-likert-opt-label">' + trial.questions[i].labels[j] + '</label></li>';
      }
      options_string += '</ul>';
      html += options_string;
    }

    // add submit button
    html += '<input type="submit" id="custom-survey-likert-next" class="custom-survey-likert jspsych-btn" value="'+trial.button_label+'"></input>';

    html += '</form>'

    display_element.innerHTML = html;

    display_element.querySelector('#custom-survey-likert-form').addEventListener('submit', function(e){
      e.preventDefault();
      // measure response time
      var endTime = performance.now();
      var response_time = endTime - startTime;

      // create object to hold responses
      var question_data = {};
      var matches = display_element.querySelectorAll('#custom-survey-likert-form .custom-survey-likert-opts');
      for(var index = 0; index < matches.length; index++){
        var id = matches[index].dataset['radioGroup'];
        var el = display_element.querySelector('input[name="' + id + '"]:checked');
        if (el === null) {
          var response = "";
        } else {
          var response = parseInt(el.value);
        }
        var obje = {};
        obje[id] = response;
        Object.assign(question_data, obje);
      }

      // save data
      var trial_data = {
        "rt": response_time,
        "responses": JSON.stringify(question_data)
      };

      display_element.innerHTML = '';

      // next trial
      jsPsych.finishTrial(trial_data);
    });

    var startTime = performance.now();
  };

  return plugin;
})();
