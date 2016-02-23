$( document ).ready(function() {
	
	if ($('#changeScheduler').length > 0) {
		$('#changeScheduler').click(function() {
			var $btn = $(this);
			sendRequest("/admintool/quartz/changeRunningState", "GET", "text", function(result) {
				$btn.text(result);
				switchClass($btn, 'btn-success', 'btn-danger');
			});
		});
	}
	
	$('.changeTrigger').each(function() {
		 var $el = $(this);
		 $el.click(function(){
			 //changeTrigger(this);
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
		 $el.click(function(){
			 doActionOnJob(this, '/admintool/quartz/triggerJob', function (result, $btn) {
				 if (result == 'true') {
					 location.reload();
				 }
			 });
		 });
	});
	$('.interruptJob').each(function() {
		 var $el = $(this);
		 $el.click(function(){
			 doActionOnJob(this, '/admintool/quartz/interruptJob', function (result, $btn) {
				 if (result == 'true') {
					 location.reload();
				 }
			 });
		 });
	});
});


function changeTrigger(btn) {
	var $btn = $(btn);
	var idAr = $btn.attr('id').split('_');
	
	var url = "/admintool/quartz/changeTriggerState?groupName=" + idAr[0] + "&jobName=" + idAr[1];
	if (idAr.length == 3) {
		url += "&triggerName=" + idAr[2];
	}
	sendRequest(url, "GET", "text", function(result) {
		if (result == 'true') {
			switchClass($btn, 'btn-success', 'btn-warning');
			$btn.text() == 'running' ? $btn.text('paused') : $btn.text('running');
		}
	});
}

function doActionOnJob(btn, urlRefix, innerCallback) {
	var $btn = $(btn);
	var idAr = $btn.attr('id').split('_');
	var url = urlRefix + "?groupName=" + idAr[0] + "&jobName=" + idAr[1];
	if (idAr.length == 3) {
		url += "&triggerName=" + idAr[2];
	}
	sendRequest(url, "GET", "text", function(result) {
		if (null != innerCallback && (typeof innerCallback === 'function')) {
			innerCallback(result, $btn)
		}
	});
}