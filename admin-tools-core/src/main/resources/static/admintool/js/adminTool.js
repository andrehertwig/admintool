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
	
	name: "adminTool",
	
	init: function(el, options) {
		 // save this element for faster queries
        this.element = $(el);
        
        // save options if there are any
        this.options = options;
        
        // bind if the element is destroyed
        this.element.bind("destroyed", $.proxy(this.teardown, this));
        
        // save this instance in jQuery data
        $.data(el, this.name, this);
        
        this.postInit();
	},
	
	postInit: function() {
		//abstract function to override
	},
	
	 // call destroy to teardown the controller while leaving the element
    destroy: function() {
        this.element.unbind("destroyed", this.teardown);
        this.teardown();
        this.unbind();
    },
    
    // remove all the functionality of this tabs widget
    teardown: function() {
        $.removeData(this.element[0], this.name);
        // clear references to this element
        this.element = null;
    },
    
    unbind : function() {
    	//abstract function to override
    },
    
    reloadPage: function() {
		location.reload();
	},
	
	sendRequest: function (query, callback) {
		var context = $('#webContext').attr('href');
		$.ajax({
			url: context + query.url,
			dataType: query.dataType || 'json',
			type: query.requestType || 'GET',
			data: query.data || null,
			contentType: query.contentType || 'application/json; charset=UTF-8',
			error: function( xhr, status, errorThrown ) {
				if (query.showModalOnError) {
					AdminTool.Core.prototype.showErrorModal(query.erroModalHeadline, query.errorModalText);
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
	
	showErrorModal: function(headline, text) {
		if (null == headline || headline === undefined) {
			headline = "Error";
		}
		if (null == text || text === undefined) {
			text = "An Error has been occurred while sending XHR request";
		}
		getByID('admintoolErrorLabel').html('<i class="icon fa fa-ban"></i>' + headline);
		getByID('admintoolErrorBody').html(text);
		getByID('admintoolError').modal();
	}
});

$.pluginMaker(AdminTool.Core);

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
	var context = $('#webContext').attr('href');
	$.ajax({
		url: context + serviceUrl,
		dataType: dataType,
		type: requestType,
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
