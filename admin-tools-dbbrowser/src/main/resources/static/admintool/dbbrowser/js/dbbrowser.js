var datasourceNames = null;

$( document ).ready(function() {
	
	getStaticObjects();
	
});

function getStaticObjects() {
	if (null == datasourceNames) {
		sendRequest('/admintool/dbbrowser/getDatasourceNames', "GET", "json", function (result) {
			datasourceNames = result;
		});
	}
}
