var instructionSets = null;
var calendarNames = null;
var timeZones = null;
var defaultTimeZone = null;
//var repeatIntervalUnits = null;
var actualTrigger = null;
var actualJob = null;
var intervalId = null;
var dateReplacePattern = /(\d{2})\.(\d{2})\.(\d{4})/;

window.onbeforeunload = closingCode;
function closingCode() {
	instructionSets = null;
	calendarNames = null;
	//repeatIntervalUnits = null;
	actualTrigger = null;
	actualJob = null;
	return null;
}

$( document ).ready(function() {
	
	$('[data-toggle="tooltip"]').tooltip();
	//http://bootstrap-datepicker.readthedocs.org/
	$('.datepicker').datepicker({
		format: 'dd.mm.yyyy',
		startDate: new Date(),
		todayHighlight: true
	});
	//http://jdewit.github.com/bootstrap-timepicker 
	$('#startTime').timepicker({
		showSeconds: true,
		secondStep: 1,
        minuteStep: 1,
        maxHours: 24,
        showMeridian: false
    });
	
	getStaticObjects();
	
	if ($('#changeScheduler').length > 0) {
		$('#changeScheduler').click(function() {
			var $btn = $(this);
			sendRequest("/admintool/quartz/changeRunningState", "GET", "text", function(result) {
				$btn.text(result);
				switchClass($btn, 'btn-success', 'btn-danger');
			});
		});
	}
	
	if ($("#quartzJobsInc").length > 0) {
		initHandlers();
		$('#reloadInclude').click(function() {reloadInclude();});
	}
	if ($("#autoreload").length > 0) {
		$('#autoreload').click(function() {
			if (null == intervalId) {
				startPermanentReload();
			} else {
				stopPermanentReload();
			}
			switchClass($('#autoreload'), 'text-success', 'text-danger');
		});
	}
	
	$('#jobModal').on('hidden.bs.modal', function (e) {
		actualTrigger = null;
		actualJob = null;
		//clear the form
		$('.form-control').each(function() {
			if (this.tagName.toLowerCase() == 'textarea') {
				$(this).text('');
			} else {
				$(this).val('');
			}
		});
	})
	
});

function initHandlers() {
	$('.changeTriggerState').each(function() {
		var $el = $(this);
		$el.click(function() {
			doActionOnJob(this, '/admintool/quartz/changeTriggerState', function (result, $btn) {
				if (result == 'true') {
					if ($btn.text() == 'running') {
						$btn.text('paused')
						switchClass($btn, 'btn-success', 'btn-warning');
					} else if($btn.text() == 'pending') {
						$btn.text('paused')
						switchClass($btn, 'btn-info', 'btn-warning');
					} else {
						$btn.text('pending')
						switchClass($btn, 'btn-info', 'btn-warning');
					}
				}
			});
		});
	});
	$('.executeJob').each(function() {
		var $el = $(this);
		$el.click(function() {
			doActionOnJob(this, '/admintool/quartz/executeJob', function (result, $btn) {
				if (result == 'true') {
					reloadIncludeDelayed(500);
				}
			});
		});
	});
	$('.interruptJob, .interruptTrigger').each(function() {
		var $el = $(this);
		$el.click(function() {
			doActionOnJob(this, '/admintool/quartz/interruptJob', function (result, $btn) {
				if (result == 'true') {
					reloadIncludeDelayed(500);
				}
			});
		});
	});
	$('.removeTrigger').each(function() {
		var $el = $(this);
		$el.click(function() {
			var btn = this;
			$('#btn_confirm').off();
			$('#btn_confirm').click(function() {
				doActionOnJob(btn, '/admintool/quartz/removeTrigger', function (result, $btn) {
					if (result == 'true') {
						location.reload();
					}
				});
			});
			$('#confirmModal').modal('show');
		});
	});
	$('.changeTrigger').each(function() {
		var $el = $(this);
		$el.click(function() {
			var $btn = $(this);
			actualTrigger = $btn;
			getStaticObjects();
			//initially show all inputs
			switchVisibility(null, ['job', 'trigger']);
			//after that we have to switch between special fields for new and changeable inputs
			switchVisibility('new_trigger', 'edit_trigger');
			 
			sendRequest(buildParameterizedUrl('/admintool/quartz/getTriggerInfo', $btn), "GET", "json", function (trigger) {
				if (trigger && null != trigger && 'null' != trigger) {
					
					fillJobData(trigger);
					fillCommonTriggerData(trigger);
					disableJobFields(true);
					if (trigger.triggerType == 'CRON') {
						$('#cronExpression').val(trigger.cronExpression);
						$('#timeZone').val(trigger.timeZone);
					}
					if (trigger.triggerType == 'SIMPLE' || trigger.triggerType == 'DAILY') {
						$('#repeatCount').val(trigger.repeatCount);
					}
					if (trigger.triggerType == 'SIMPLE' || trigger.triggerType == 'CALENDAR' || trigger.triggerType == 'DAILY') {
						$('#repeatInterval').val(trigger.repeatInterval);
					}
					if (trigger.triggerType == 'CALENDAR' || trigger.triggerType == 'DAILY') {
						$('#repeatIntervalUnit').val(trigger.repeatIntervalUnit);
					}
					
					initTriggerTypeList();

					switchVisibilityOnTrigger(trigger);
					
					$('#btn_save').off();
					$('#btn_save').click(function() {
						save('changeTrigger');
					});
					$('#jobModalLabel').html('Change Trigger');
					reloadValidator('jobTriggerForm');
					$('#jobModal').modal('show');
				}
			});
		});
	});//changeTrigger
	
	$('.addTrigger').each(function() {
		var $el = $(this);
		$el.click(function() {
			var $btn = $(this);
			actualJob = $btn;
			actualTrigger = null;
			getStaticObjects();
			hideSelectWithoutOptions('#calendarName');
			//initially show all inputs
			switchVisibility(null, ['job', 'trigger']);
			//after that we have to switch between special fields for new and changeable inputs
			switchVisibility('edit_trigger', 'new_trigger');
			
			sendRequest(buildParameterizedUrl('/admintool/quartz/getTriggerInfo', $btn), "GET", "json", function (trigger) {
				if (trigger && null != trigger && 'null' != trigger) {
					fillJobData(trigger);
					disableJobFields(true);
					
					initTriggerTypeList();
					//init form
					var select = getByID('triggerTypeList').val('CRON');
					
					while (null == instructionSets) {
						//wait until... instructionSets have been loaded
					}
					for (var i = 0, len = instructionSets.length; i < len; i++) {
						if (instructionSets[i].triggerType == select.val()) {
							fillSelectFromMap('#misfireInstruction', instructionSets[i].misfireInstructions, true);
						}
					}
					switchVisibility(['cron', 'simple', 'daily', 'calendar'], select.val());
					getByID('timeZone').val(defaultTimeZone);
					var time = new Date()
					$('.datepicker').datepicker('update', time);
					$('#startTime').timepicker('setTime', time.getHours() + ':' + time.getMinutes() + ':' + time.getSeconds());
					
					getByID('jobModalLabel').html('Add Trigger');
					
					getByID('btn_save').off();
					getByID('btn_save').click(function() {
						save('addTrigger');
					});
					reloadValidator('jobTriggerForm');
					getByID('jobModal').modal('show');
				}
			});
		
		});
	});//addTrigger
	
	$('.changeJob').each(function() {
		var $el = $(this);
		$el.click(function() {
			var $btn = $(this);
			actualJob = $btn;
			getStaticObjects();
			//hide trigger specific inputs
			switchVisibility('trigger', 'job');
			sendRequest(buildParameterizedUrl('/admintool/quartz/getTriggerInfo', $btn), "GET", "json", function (trigger) {
				if (trigger && null != trigger && 'null' != trigger) {
					fillJobData(trigger);
					disableJobFields(false);
					$('#btn_save').off();
					$('#btn_save').click(function() {
						save('changeJob');
					});
					$('#jobModalLabel').html('Change Job');
					reloadValidator('jobTriggerForm');
					$('#jobModal').modal('show');
				}
			});
		});
	});//editJob
}

function disableJobFields(disable) {
	getByID('jobGroup').prop('disabled', disable);
	getByID('jobName').prop('disabled', disable);
	getByID('description').prop('disabled', disable);
	getByID('jobGroup').prop('required', !disable);
	getByID('jobName').prop('required', !disable);
	
	getByID('triggerGroup').prop('required', disable);
	getByID('triggerName').prop('required', disable);
}

function initTriggerTypeList() {
	getByID('triggerTypeList').change(function() {
		var select = $(this);
		//change misfire instructions
		for (var i = 0, len = instructionSets.length; i < len; i++) {
			if (instructionSets[i].triggerType == select.val()) {
				fillSelectFromMap('#misfireInstruction', instructionSets[i].misfireInstructions, true);
				fillSelectFromMap('#repeatIntervalUnit', instructionSets[i].repeatIntervalUnits, true);
			}
		}
		//change visibilities
		switchVisibility(['cron', 'simple', 'daily', 'calendar'], select.val());
	});
}

function startPermanentReload() {
	intervalId = setInterval(function() {
	    reloadInclude()
	}, 15000);
}
function stopPermanentReload() {
	clearInterval(intervalId);
	intervalId = null;
}

function reloadIncludeDelayed(delay) {
	setTimeout(reloadInclude, delay);
}

function reloadInclude() {
	$('#reloadInclude').addClass('fa-spin');
	sendRequest("/admintool/quartz/quartzJobsInc", "GET", "text", function(result) {
		$("#quartzJobsInc").html(result);
		initHandlers();
		$('#reloadInclude').removeClass('fa-spin');
	});
}

function reloadValidator(formId) {
	getByID(formId).validator('destroy');
	//validator doesn't remove everything
	getByID(formId).find('.form-control-feedback').removeClass('glyphicon-remove');
	getByID(formId).validator();
}

function hasValidationErrors(id) {
	return $(getByID(id)).data('bs.validator').hasErrors();
}

function save(type) {
	
	getByID('jobTriggerForm').validator('validate');
	if (hasValidationErrors('jobTriggerForm')) {
		return;
	}
	
	var triggerModel = {};
	
	if (type == 'changeJob' || type == 'addTrigger' || type == 'changeTrigger') {
		triggerModel.jobGroup = getByID('jobGroup').val();
		triggerModel.jobName = getByID('jobName').val();
		triggerModel.description = getByID('description').val();
	}
	
	if (type == 'addTrigger' || type == 'changeTrigger') {
		triggerModel.triggerGroup = getByID('triggerGroup').val();
		triggerModel.triggerName = getByID('triggerName').val();
		triggerModel.triggerDescription = getByID('triggerDescription').val();
		triggerModel.triggerType = getByID('triggerTypeList').val();
		
		triggerModel.cronExpression = getByID('cronExpression').val();
		triggerModel.misfireInstruction = getByID('misfireInstruction').val();
		triggerModel.timeZone = getByID('timeZone').val();
		triggerModel.repeatCount = getByID('repeatCount').val();
		triggerModel.repeatInterval = getByID('repeatInterval').val();
		triggerModel.repeatIntervalUnit = getByID('repeatIntervalUnit').val();
		triggerModel.priority = getByID('priority').val();
		
		triggerModel.startTime = new Date(getByID('startDate').val().replace(dateReplacePattern,'$3-$2-$1') 
				+ 'T' + getByID('startTime').val());
	}
	
	var org = null;
	type == 'changeTrigger' ? org = actualTrigger : org = actualJob;
	var idAr = org.attr('id').split('_');
	triggerModel.originalJobGroup = idAr[1];
	triggerModel.originalJobName = idAr[2];
	if (idAr.length > 3) {
		triggerModel.originalTriggerGroup = idAr[3];
		triggerModel.originalTriggerName = idAr[4];
	}
	
	$.ajax({
		url: '/admintool/quartz/' + type,
		data: JSON.stringify(triggerModel),
		dataType: "text",
		type: 'POST',
		contentType:'application/json; charset=UTF-8' ,
		error: function( xhr, status, errorThrown ) {
	        alert( "Sorry, there was a problem!" );
	        if (console) {
	        	console.log( "Error: " + errorThrown );
		        console.log( "Status: " + status );
		        console.dir( xhr );
	        }
		}
	}).done(function (data) {
		if ('true' === data) {
			reloadInclude();
			$('#jobModal').modal('hide');
		}
	});
}

function getStaticObjects() {
	if (null == instructionSets) {
		sendRequest('/admintool/quartz/getInstructionSets', "GET", "json", function (result) {
			instructionSets = result;
		});
	}
	if (null == calendarNames) {
		sendRequest('/admintool/quartz/getCalendarNames', "GET", "json", function (result) {
			calendarNames = result;
			fillSelectFromList('#calendarName', result, true);
		});
	}
	if (null == defaultTimeZone) {
		sendRequest('/admintool/quartz/getDefaultTimeZone', "GET", "text", function (result) {
			defaultTimeZone = result;
		});
	}
	if (null == timeZones) {
		sendRequest('/admintool/quartz/getTimeZones', "GET", "json", function (result) {
			
			timeZones = result;
			fillSelectFromList('#timeZone', result, true);
		});
	}
}

function fillJobData(trigger) {
	$('#jobGroup').val(trigger.jobGroup);
	$('#jobName').val(trigger.jobName);
	$('#description').text(trigger.description);
	$('#triggerGroup').val(trigger.triggerGroup != null ? trigger.triggerGroup : trigger.jobGroup);
	$('#triggerName').prop('required', false);
}

function fillCommonTriggerData(trigger) {
	$('#triggerGroup').val(trigger.triggerGroup);
	$('#triggerName').val(trigger.triggerName);
	$('#triggerName').prop('required', true);
	$('#triggerDescription').text(trigger.triggerDescription);
	var select = getByID('triggerTypeList').val(trigger.triggerType);
	 
	var sd = new Date(trigger.startTime);
	$('#startTime').timepicker('setTime', sd.getHours() + ':' + sd.getMinutes() + ':' + sd.getSeconds());
	$('.datepicker').datepicker('update', sd);
	
	if (null != trigger.calendarName && $('#calendarName').length > 0) {
		$('#calendarName').val(trigger.calendarName);
	}
	hideSelectWithoutOptions('#calendarName');
	
	getByID('timeZone').val(trigger.timeZone);
	
	fillSelectFromMap('#misfireInstruction', trigger.misfireInstructions, true);
	fillSelectFromMap('#repeatIntervalUnit', trigger.repeatIntervalUnits, true);
	hideSelectWithoutOptions('#misfireInstruction');
}

function switchVisibilityOnTrigger(trigger) {
	switchVisibility(trigger.types, trigger.triggerType);
}

function switchVisibility(hideClass, showClass) {
	//hide unnecessary input rows
	if (null != hideClass) {
		if (Array.isArray(hideClass)) {
			$.each(hideClass, function (i, item) {
				var hc = getByClazz(item.toLowerCase());
				findInputsAndSetRequired(hc, true);
				hc.hide();
			});
		} else {
			var hc = getByClazz(hideClass.toLowerCase());
			findInputsAndSetRequired(hc, true);
			hc.hide();
		}
	}
	
	//show necessary input rows
	if (null != showClass) {
		if (Array.isArray(showClass)) {
			$.each(showClass, function (i, item) {
				var sc = getByClazz(item.toLowerCase());
				findInputsAndSetRequired(sc, true);
				sc.show();
			});
		} else {
			var sc = getByClazz(showClass.toLowerCase());
			findInputsAndSetRequired(sc, true);
			sc.show();
		}
	}
	
	reloadValidator('jobTriggerForm');
}

function findInputsAndSetRequired($formGroup, required) {
	$formGroup.find('.form-control').each(function() {
		var $input = $(this);
		if (!$input.hasClass('notRequired')) {
			$input.prop('required', required);
		}
	});
}

function fillSelectFromList(id, list, clearBefore) {
	var $select = getByID(id);
	if (clearBefore) {
		$select.html('');
	}
	$.each(list, function (i, item) {
		$select.append($('<option>', { 
			 value: item,
			 text : item 
		 }));
	 });
}
function fillSelectFromMap(id, map, clearBefore) {
	if (null == map || map === undefined) {
		return;
	}
	var $select = getByID(id);
	if (clearBefore) {
		$select.html('');
	}
	$.each(map, function (key, value) {
		$select.append($('<option>', { 
			value: value,
			text : key 
		}));
	});
}

function hideSelectWithoutOptions(id) {
	if ($(getID(id) + ' > option').length == 0) {
		getByID(id).hide();
	} else {
		getByID(id).show();
	}
}

function doActionOnJob(btn, urlRefix, innerCallback) {
	var $btn = $(btn);
	var url = buildParameterizedUrl(urlRefix, $btn);
	sendRequest(url, "GET", "text", function(result) {
		if (null != innerCallback && (typeof innerCallback === 'function')) {
			innerCallback(result, $btn);
		}
	});
}

function buildParameterizedUrl(urlRefix, $btn) {
	var idAr = $btn.attr('id').split('_');
//	var url = urlRefix + "?groupName=" + idAr[1] + "&jobName=" + idAr[2];
	var url = urlRefix + '/' + idAr[1] + '/' + idAr[2]
	if (idAr.length > 3 && idAr[4] != 'all') {
//		url += "&triggerName=" + idAr[3];
		url += "/" + idAr[3] + "/" + idAr[4];
	}
	return url;
}
