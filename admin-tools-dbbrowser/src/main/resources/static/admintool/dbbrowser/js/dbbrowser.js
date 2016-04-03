var datasourceNames = null;
var metaData = {};
var codeMirrors = {};

$( document ).ready(function() {
	
	getStaticObjects();
	initCheckboxes($('#tab_1'));
	initTab(1);
});

function initCheckboxes($object) {
	$object.find('input').iCheck({
		checkboxClass: 'icheckbox_minimal',
		radioClass: 'iradio_minimal',
//		increaseArea: '20%' // optional
	});
}

function initTab(number) {
	$("#tabInclude_" + number);
	
	$("#execute_" + number).off();
	$("#execute_" + number).click(function() {
		executeQuery(number);
	});
	
	initCodeMirror("statement", number, 'text/x-sql');
}

function executeQuery(number) {
	
	var query = {
		tab : number,
		datasourceName : $('#datasourceName_' + number).val(),
		clobEncoding :  $('#clobEncoding_' + number).val(),
		showClobs :  $('#showClobs_' + number).prop("checked"),
		showBlobs :  $('#showBlobs_' + number).prop("checked"),
		maxResults :  $('#maxResults_' + number).val(),
		statement :  codeMirrors[number].getValue()
	};
	
	$.ajax({
		url: '/admintool/dbbrowser/executeQuery',
		data: JSON.stringify(query),
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
		var id = $(data).attr('id');
		var tabNumber = id.split('_')[1];
		var $tabContent = $("#tab_" + tabNumber);
		
		$tabContent.html(data);
		initCheckboxes($tabContent);
		initTab(tabNumber);
		initCodeMirror('statement' , tabId, 'text/x-sql');
		
		div = null;
	});
}

function getStaticObjects() {
	if (null == datasourceNames) {
		sendRequest('/admintool/dbbrowser/getDatasourceNames', "GET", "json", function (result) {
			datasourceNames = result;
		});
	}
	getMetaData(1);
}

function getMetaData(number) {
	if (!metaData.hasOwnProperty(number.toString())) {
		sendRequest('/admintool/dbbrowser/getMetaData/' + $('#datasourceName_' + number).val(), "GET", "json", function (result) {
			metaData[number.toString()] = result;
		});
	}
}

function initCodeMirror(id, number, mode) {
	var cm = CodeMirror.fromTextArea(document.getElementById(id + "_" + number), {
		mode: mode,
        lineNumbers: true,
        indentWithTabs: true,
        smartIndent: true,
		styleActiveLine: true,
		matchBrackets: true,
		viewportMargin: Infinity,
		autofocus: true,
		extraKeys: {"Ctrl-Space": "autocomplete"}
	// todo hin options
//		hintOptions: {tables: {
//	    	users: {name: null, score: null, birthDate: null},
//	    	countries: {name: null, population: null, size: null}
//	    }}
	});
	codeMirrors[number] = cm;
	setTimeout(refreshCodeMirror, 100, number);
}

function refreshCodeMirror(number) {
	codeMirrors[number].refresh();
}
