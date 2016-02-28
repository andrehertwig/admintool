var misfireInstructions = null;
var calendarNames = null;
var repeatIntervalUnits = null;
var actualTrigger = null;
var actualJob = null;

window.onbeforeunload = closingCode;
function closingCode() {
	misfireInstructions = null;
	calendarNames = null;
	repeatIntervalUnits = null;
	actualTrigger = null;
	actualJob = null;
	return null;
}

$( document ).ready(function() {
	
	$('[data-toggle="tooltip"]').tooltip();
	$('.datepicker').datepicker();
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
	
	$('.changeTriggerState').each(function() {
		var $el = $(this);
		$el.click(function() {
			doActionOnJob(this, '/admintool/quartz/changeTriggerState', function (result, $btn) {
				if (result == 'true') {
					switchClass($btn, 'btn-success', 'btn-warning');
					$btn.text() == 'running' ? $btn.text('paused') : $btn.text('running');
				}
			});
		});
	});
	$('.executeJob').each(function() {
		var $el = $(this);
		$el.click(function() {
			doActionOnJob(this, '/admintool/quartz/executeJob', function (result, $btn) {
				if (result == 'true') {
					location.reload();
				}
			});
		});
	});
	$('.interruptJob, .interruptTrigger').each(function() {
		var $el = $(this);
		$el.click(function() {
			doActionOnJob(this, '/admintool/quartz/interruptJob', function (result, $btn) {
				if (result == 'true') {
					location.reload();
				}
			});
		});
	});
	$('.removeTrigger').each(function() {
		var $el = $(this);
		$el.click(function() {
			var btn = this;
			$('#btn_confirm').unbind();
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
					
					if (trigger.type == 'CRON') {
						$('#cronExpression').val(trigger.cronExpression);
						$('#timeZone').val(trigger.timeZone);
					}
					if (trigger.type == 'SIMPLE' || trigger.type == 'DAILY') {
						$('#repeatCount').val(trigger.repeatCount);
					}
					if (trigger.type == 'SIMPLE' || trigger.type == 'CALENDAR' || trigger.type == 'DAILY') {
						$('#repeatInterval').val(trigger.repeatInterval);
					}
					if (trigger.type == 'CALENDAR' || trigger.type == 'DAILY') {
						$('#repeatIntervalUnit').val(trigger.repeatIntervalUnit);
					}
					 
					switchVisibilityOnTrigger(trigger);
					
					$('#btn_save').unbind();
					$('#btn_save').click(function() {
						save('changeTrigger');
					});
					$('#jobModal').modal('show');
				}
			});
		});
	});//changeTrigger
	
	$('.addTrigger').each(function() {
		var $el = $(this);
		$el.click(function() {
			var $btn = $(this);
			actualTrigger = null;
			getStaticObjects();
			hideSelectWithoutOptions('#calendarName');
			//initially show all inputs
			switchVisibility(null, ['job', 'trigger']);
			//after that we have to switch between special fields for new and changeable inputs
			switchVisibility('edit_trigger', 'new_trigger');
			
			var idAr = $btn.attr('id').split('_');
			$('#jobGroup').val(idAr[1]);
			$('#jobName').val(idAr[2]);
			$('#triggerGroup').val(idAr[1]);
			
			getByID('triggerTypeList').change(function() {
				var select = $(this);
				//change misfire instructions
				for (var i = 0, len = misfireInstructions.length; i < len; i++) {
					if (misfireInstructions[i].type == select.val()) {
						fillSelectFromMap('#misfireInstruction', misfireInstructions[i].misfireInstructions, true);
					}
				}
				//change visibilities
				switchVisibility(['cron', 'simple', 'daily', 'calendar'], select.val());
			});
			//init form
			var select = getByID('triggerTypeList').val('CRON');
			
			while (null == misfireInstructions) {
				//wait... misfireInstructions have been loaded
			}
			for (var i = 0, len = misfireInstructions.length; i < len; i++) {
				if (misfireInstructions[i].type == select.val()) {
					fillSelectFromMap('#misfireInstruction', misfireInstructions[i].misfireInstructions, true);
				}
			}
			switchVisibility(['cron', 'simple', 'daily', 'calendar'], select.val());
			
			$('#jobModal').modal('show');
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
					$('#btn_save').unbind();
					$('#btn_save').click(function() {
						save('editJob');
					});
					$('#jobModal').modal('show');
				}
			});
		});
	});//editJob
});

function save(type ) {
	
}

function getStaticObjects() {
	if (null == misfireInstructions) {
		sendRequest('/admintool/quartz/getMisfireInstructions', "GET", "json", function (result) {
			misfireInstructions = result;
		});
	}
	if (null == calendarNames) {
		sendRequest('/admintool/quartz/getCalendarNames', "GET", "json", function (result) {
			calendarNames = result;
			fillSelectFromList('#calendarName', result, true);
		});
	}
	if (null == repeatIntervalUnits) {
		sendRequest('/admintool/quartz/getIntervalUnits', "GET", "json", function (result) {
			repeatIntervalUnits = result;
			fillSelectFromList('#repeatIntervalUnit', result, true);
		});
	}
}

function fillJobData(trigger) {
	$('#jobGroup').val(trigger.jobGroup);
	$('#jobName').val(trigger.jobName);
	$('#description').text(trigger.description);
}

function fillCommonTriggerData(trigger) {
	$('#triggerGroup').val(trigger.triggerGroup);
	$('#triggerName').val(trigger.triggerName);
	$('#triggerDescription').text(trigger.triggerDescription);
	$('#triggerType').val(trigger.type);
	 
	var sd = new Date(trigger.startTime);
	$('#startTime').timepicker('setTime', sd.getHours() + ':' + sd.getMinutes() + ':' + sd.getSeconds());
	$('.datepicker').datepicker('update', sd);
	
	if (null != trigger.calendarName && $('#calendarName').length > 0) {
		$('#calendarName').val(trigger.calendarName);
	}
	hideSelectWithoutOptions('#calendarName');
	
	fillSelectFromMap('#misfireInstruction', trigger.misfireInstructions, true);
	hideSelectWithoutOptions('#misfireInstruction');
}

function switchVisibilityOnTrigger(trigger) {
	switchVisibility(trigger.types, trigger.type);
}

function switchVisibility(hideClass, showClass) {
	//hide unnecessary input rows
	if (null != hideClass) {
		if (Array.isArray(hideClass)) {
			$.each(hideClass, function (i, item) {
				getByClazz(item.toLowerCase()).hide();
			});
		} else {
			getByClazz(hideClass.toLowerCase()).hide();
		}
	}
	
	//show necessary input rows
	if (null != showClass) {
		if (Array.isArray(showClass)) {
			$.each(showClass, function (i, item) {
				getByClazz(item.toLowerCase()).show();
			});
		} else {
			getByClazz(showClass.toLowerCase()).show();
		}
	}
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
	var url = urlRefix + "?groupName=" + idAr[1] + "&jobName=" + idAr[2];
	if (idAr.length == 4 && idAr[3] != 'all') {
		url += "&triggerName=" + idAr[3];
	}
	return url;
}
