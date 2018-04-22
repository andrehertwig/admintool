//http://blog.bitovi.com/writing-the-perfect-jquery-plugin/
$.pluginMaker = function(plugin) {
    $.fn[plugin.prototype.name] = function(options) {
        
        var args = $.makeArray(arguments),
            after = args.slice(1);

        return this.each(function() {
            
            // see if we have an instance
            var instance = $.data(this, plugin.prototype.name);
            if (instance) {
                
                // call a method on the instance
                if (typeof options == "string") {
                	if (after.length == 0) {
                		instance[options].call(instance, after);
                	} else {
                		instance[options].apply(instance, after);
                	}
                } else if (instance.update) {
                    
                    // call update on the instance
                    instance.update.apply(instance, args);
                }
            } else {
                
                // create the plugin
                new plugin(this, options);
            }
        })
    };
};

AdminTool = {};

AdminTool.Core = function(el, options) {
	if (el) {
        this.init(el, options)
    }
}

$.extend(AdminTool.Core.prototype, {
	
	// my name :)
	name: "adminTool",
	
	/**
	 * the init function
	 */
	init: function(el, options) {
		 // save this element for faster queries
        this.element = $(el);
        this.elementId = this.element.attr('id');
        
        // save options if there are any
        this.options = {
        	errorModalId : "admintoolError",
        	errorModalLabelId : "admintoolErrorLabel",
        	errorModalTextId : "admintoolErrorBody",
        	confirmModalId : "admintoolConfirmModal",
        	confirmModalLabelId : "admintoolConfirmModalLabel",
        	confirmModalTextId : "admintoolConfirmModalText",
        	confirmModalButtonId :"btn_admintool_confirm"
        };
        
        // bind if the element is destroyed
        this.element.bind("destroyed", $.proxy(this.teardown, this));
        
        // save this instance in jQuery data
        $.data(el, this.name, this);
        
        this.initConfirmModal();
        
        //overriding options before extending plugins maybe pushed own config
        this.options = $.extend( this.options, options );
        
        this.postInit();
	},
	
	/**
	 * abstract function will be called within this.init() before options are getting merged
	 */
	postInit: function() {
		//abstract function to override
	},
	
	/**
	 * call destroy to teardown the controller while leaving the element
	 */
    destroy: function() {
        this.element.unbind("destroyed", this.teardown);
        this.teardown();
        this.unbind();
    },
    
    /**
     * removes all the functionality of this widget
     */
    teardown: function() {
        $.removeData(this.element[0], this.name);
        // clear references to this element
        this.element = null;
    },
    
    /**
     * abstract function which will be called while this.destroy()
     */
    unbind : function() {
    	//abstract function to override
    },
    
    /**
     * reloads the page
     */
    reloadPage: function() {
		location.reload();
	},
	
	/**
	 * Sends a Ajax Request with CSRF header token. Per default it's a GET.
	 * 
	 *  @param query - the query object
	 *  @param query.url - (required) URI to call
	 *  @param query.dataType - (optional) Default: json
	 *  @param query.requestType - (optional) Default: GET
	 *  @param query.data - (optional) Default: null
	 *  @param query.contentType - (optional) Default: application/json; charset=UTF-8
	 *  @param query.showModalOnError - (optional) if true the default error modal with optional message will be shown
	 *  @param query.erroModalHeadline - (optional) headline for error modal
	 *  @param query.errorModalText - (optional) text for error modal
	 *  
	 *  @param callback(data, query) - callback function with two parameters.first is the data, second the original query param.
	 */
	sendRequest: function (query, callback) {
		var context = getWebContext();
		var token = $("meta[name='_csrf']").attr("content");
		var header = $("meta[name='_csrf_header']").attr("content");
		var admintool = this;
		$.ajax({
			url: context + query.url,
			dataType: query.dataType || 'json',
			type: query.requestType || 'GET',
			data: query.data || null,
			contentType: query.contentType || 'application/json; charset=UTF-8',
			beforeSend: function(xhr, settings) {
				xhr.setRequestHeader(header, token);
			},
			error: function( xhr, status, errorThrown ) {
				if (query.showModalOnError) {
//					AdminTool.Core.prototype.showErrorModal(query.erroModalHeadline, query.errorModalText);
					if (query.showXHRErrorInModal) {
						var headline = "HTTP " + status + " " + xhr.status;
						var text = xhr.responseText;
						if (xhr.responseJSON) {
							text = '<pre>' + JSON.stringify(xhr.responseJSON, null, "\t") + '</pre>';
						}
						admintool.showErrorModal(headline, text);
					} else {
						admintool.showErrorModal(query.erroModalHeadline, query.errorModalText);
					}
				}
				if (console) {
					console.log( "Error: " + errorThrown );
					console.log( "Status: " + status );
		        	console.dir( xhr );
				}
			}
		}).done(function (data) {
			callback(data, query);
		});
	},
	
	/**
	 * shows the default error modal which will be identified by "this.options.errorModalId"
	 * 
	 * @param headline - headline for error modal (optional)
	 * @param text - text for error modal (optional)
	 */
	showErrorModal: function(headline, text) {
		if (null == headline || headline === undefined) {
			headline = "Error";
		}
		if (null == text || text === undefined) {
			text = "An Error has been occurred while sending XHR request";
		}
		getByID(this.options.errorModalLabelId).html('<i class="icon fa fa-ban"></i>' + headline);
		getByID(this.options.errorModalTextId).html(text);
		getByID(this.options.errorModalId).modal();
	},
	
	appendToErrorModal: function(text) {
		getByID(this.options.errorModalTextId).append(text);
	},
	
	/**
	 * init function for confirm modal, whichs calls the unbindConfirmModal on hide
	 */
	initConfirmModal: function() {
		getByID(this.options.confirmModalId).on('hidden.bs.modal', $.proxy(this.unbindConfirmModal, this));
	},
	
	/**
	 * unbinds the click funcktions from "this.options.confirmModalButtonId"
	 */
	unbindConfirmModal: function() {
		getByID(this.options.confirmModalButtonId).off();
	},
	
	/**
	 * shows the default confirm modal
	 * 
	 * @param confirmTitle - headline for confirm modal (optional)
	 * @param confirmMessage - text for confirm modal (optional)
	 * @param confirmCallback - a callback function for on.click "this.options.confirmModalButtonId"
	 * @param args - arguments for callback (optional)
	 */
	showConfirmModal: function(confirmTitle, confirmMessage, confirmCallback, args) {
		this.unbindConfirmModal();
		getByID(this.options.confirmModalButtonId).on('click', $.proxy(confirmCallback, this, args));
		
		if(!confirmTitle) {
			confirmTitle = 'Confirm';
		}
		if(!confirmMessage) {
			confirmMessage = 'Do you confirm?';
		}
		getByID(this.options.confirmModalLabelId).html(confirmTitle);
		getByID(this.options.confirmModalTextId).html(confirmMessage);
		getByID(this.options.confirmModalId).modal('show');
	}
});

$.pluginMaker(AdminTool.Core);

function getWebContext() {
	return $('#webContext').attr('href');
}
function getCSRFToken() {
	return $("meta[name='_csrf']").attr("content");
}
function getCSRFHeader() {
	return $("meta[name='_csrf_header']").attr("content");
}

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
 * @param serviceUrl - (required) URI to call (Web-Application context will be set automatically)
 * @param requestType - (required) GET / POST ...
 * @param dataType - (required) json / text ...
 * @param callback(responseData) - (required)  callback function with one parameter
 */
function sendRequest(serviceUrl, requestType, dataType, callback) {
	var context = getWebContext();
	var token = $("meta[name='_csrf']").attr("content");
	var header = $("meta[name='_csrf_header']").attr("content");
	$.ajax({
		url: context + serviceUrl,
		dataType: dataType,
		type: requestType,
		beforeSend: function(xhr, settings) {
			xhr.setRequestHeader(header, token);
		},
		error: function( xhr, status, errorThrown ) {
			$('#admintoolError').modal();
	        if (console) {
	        	console.log( "Error: " + errorThrown );
		        console.log( "Status: " + status );
		        console.dir( xhr );
	        }
		}
	}).done(function (data) {
		callback(data);
	});
}

(function ( $ ) {
	 
	/**
	 * removes one css class and adds the other
	 * @param $object the JQuery object
	 * @param classToRemove String
	 * @param classToAdd String
	 */
    $.fn.removeAddClass = function(classToRemove, classToAdd) {
    	var $object = $(this);
    	$object.removeClass(classToRemove);
    	$object.addClass(classToAdd);
    };
    
    /**
     * switches the css classes on the object
     * @param $object
     * @param classToRemove
     * @param classToAdd
     */
    $.fn.switchClass = function(classToCheck1, classToCheck2) {
    	var $object = $(this);
    	if ($object.hasClass(classToCheck1)) {
    		$object.removeAddClass(classToCheck1, classToCheck2);
    	} else {
    		$object.removeAddClass(classToCheck2, classToCheck1);
    	}
    };
 
}( jQuery ));


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
