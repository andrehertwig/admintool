//adminTool plugin
(function($, window, document, undefined ) {
	
	var AdminTool = function(elem, settings) {
		this.elem = elem;
	    this.$elem = $(elem);
	    this.options = settings;
	    
	    this.plugins = {};
	    
	    this._init();
	}
	AdminTool.prototype = {
			
		constructor: AdminTool,
		
		_init: function() {
			this.$elem.data( "admintool.root" , this );
		},
		
		addPlugin: function(name, plugin) {
			this.plugins[name] = plugin;
		},
		
		reloadPage: function() {
			location.reload();
		},
		sendRequest: function (query, callback) {
			$.ajax({
				url: query.url,
				dataType: query.dataType || 'json',
				type: query.requestType || 'GET',
				data: query.data || null,
				contentType: query.contentType || 'application/json; charset=UTF-8',
				error: function( xhr, status, errorThrown ) {
			        alert( "Sorry, there was a problem!" );
			        if (console) {
			        	console.log( "Error: " + errorThrown );
				        console.log( "Status: " + status );
				        console.dir( xhr );
			        }
				}
			}).done(function (data) {
				callback(data, query);
			});
		}
	
	};
	
	$.fn.admintool = function( option ) {
		var args = Array.apply(null, arguments);
	    args.shift();
	    
		return this.each(function () {
			 var $this = $(this),
		        data = $this.data('admintool.root'),
		        options = typeof option === 'object' && option;
			 
			 if (!data) {
				 return new AdminTool(this, options);
			 }
			 if (typeof option === 'string') {
		        data[option].apply(data, args);
		     }
	    });
	};
	
	$.fn.admintool.Constructor = AdminTool;
	
})(jQuery, window, document);

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
