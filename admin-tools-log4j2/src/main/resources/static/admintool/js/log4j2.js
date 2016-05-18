var leveCss = null;
var prefix = 'bg';

$( document ).ready(function() {
	//get all levelCss
	sendRequest("/admintool/log4j2/getLevelCss?prefix=" + prefix, "GET", "json", function(data) {
		leveCss = data;
	});
	if($('#parentLoggers').length > 0) {
		$('#loggers').DataTable();
	}
	if($('#loggers').length > 0) {
			$('#parentLoggers').DataTable({"initComplete": function(settings, json) {
				initEvents();
		  	}
		});
	}
	$('.removeCustomLogger').click(function(){
		sendRequest("/admintool/log4j2/removeCustomLoggers", "POST", "text", function(data) {
			location.reload();
		});
	});
});

function initEvents() {
	$('.changeLogger').each(function() {
		 var $el = $(this);
		 $el.click(function(){
			 changeLogLevel(this);
		 });
	 });
}

function changeLogLevel(link) {
	var $link = $(link);
	var level = $link.html();
	var $tr = $($link.parent().parent());
	var name = $tr.find('.logname').text();
	var serviceUrl = "/admintool/log4j2/changeLevel?loggerName=" + name + "&level=" + level;
	if ($tr.hasClass('parent')) {
		serviceUrl += "&parent=true";
	}
	var context = $('#webContext').attr('href');
	$.ajax({
		url: context + serviceUrl,
		dataType: "text",
		type: 'POST',
		error: function( xhr, status, errorThrown ) {
	        alert( "Sorry, there was a problem!" );
	        if (console) {
	        	console.log( "Error: " + errorThrown );
		        console.log( "Status: " + status );
		        console.dir( xhr );
	        }
		}
	}).done(function (data) {
		if (data == 'true') {
			var $levelTd = $tr.children('.loglevel');
			var oldlevel = $levelTd.text();
			$levelTd.removeClass(leveCss[oldlevel]);
			$levelTd.addClass(leveCss[level]);
			$levelTd.text(level);
		} else if (data == 'reload') {
			location.reload();
		} else {
			//TODO: error...
		}
	});
}