var leveCss = null;
var prefix = 'bg';

$( document ).ready(function() {
	//get all levelCss
	sendRequest("/admintool/log4j2/getLevelCss?prefix=" + prefix, "GET", "json", function(data) {
		leveCss = data;
	});
	$('.changeLogger').each(function() {
		 var $el = $(this);
		 $el.click(function(){
			 changeLogLevel(this);
		 });
	 });
	$('#removeCustomLogger').click(function(){
		sendRequest("/admintool/log4j2/removeCustomLoggers", "POST", "text", function(data) {
			location.reload();
		});
	});
});

function changeLogLevel(link) {
	var $link = $(link);
	var level = $link.html();
	var $tr = $($link.parent().parent().parent());
	var name = $($tr.children('.logname')).text();
	var serviceUrl = "/admintool/log4j2/changeLevel?loggerName=" + name + "&level=" + level;
	if ($tr.hasClass('parent')) {
		serviceUrl += "&parent=true";
	}
	
	$.ajax({
		url: serviceUrl,
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

function sendRequest(serviceUrl, requestType, dataType, callback) {
	$.ajax({
		url: serviceUrl,
		dataType: dataType,
		type: requestType,
	}).done(function (data) {
		callback(data);
	});
}