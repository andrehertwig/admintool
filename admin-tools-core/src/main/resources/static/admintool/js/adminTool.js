$( document ).ready(function() {
	if($('#reloadPage').length > 0) {
		$('#reloadPage').click(function () {
			location.reload();
		});
	}
});

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

/**
 * removes one css class and adds the other
 * @param $object the JQuery object
 * @param classToRemove String
 * @param classToAdd String
 */
function removeAddClass($object, classToRemove, classToAdd) {
	$object.removeClass(classToRemove);
	$object.addClass(classToAdd);
}

/**
 * switches the css classes on the object
 * @param $object
 * @param classToRemove
 * @param classToAdd
 */
function switchClass($object, classToCheck1, classToCheck2) {
	if ($object.hasClass(classToCheck1)) {
		removeAddClass($object, classToCheck1, classToCheck2);
	} else {
		removeAddClass($object, classToCheck2, classToCheck1);
	}
}

function getID(id) {
	return id.startsWith('#') ? id : "#"+id;  
}
function getByID(id) {
	return $(getID(id));  
}
function getClazz(clazz) {
	return clazz.startsWith('.') ? clazz : "."+clazz;  
}
function getByClazz(clazz) {
	return $(clazz.startsWith('.') ? clazz : "."+clazz);  
}

if (!String.prototype.startsWith) {
	String.prototype.startsWith = function(searchString, position) {
		position = position || 0;
		return this.indexOf(searchString, position) === position;
	};
}