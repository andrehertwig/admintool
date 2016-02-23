/**
 * JQuery function for sending a XHR 
 *  
 * @param serviceUrl
 * @param requestType
 * @param dataType
 * @param callback
 */
function sendRequest(serviceUrl, requestType, dataType, callback) {
	$.ajax({
		url: serviceUrl,
		dataType: dataType,
		type: requestType,
	}).done(function (data) {
		callback(data);
	});
}