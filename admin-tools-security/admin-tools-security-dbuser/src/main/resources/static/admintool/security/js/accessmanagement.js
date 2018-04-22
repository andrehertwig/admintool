
AdminTool.AccessManagement = function(el, options) {
	if (el) {
        this.init(el, options)
    }
}
AdminTool.AccessManagement.prototype = new AdminTool.Core();

$.extend(AdminTool.AccessManagement.prototype, {
	
	name : 'accessManagement',
	
	getIdentifier: function($button, type) {
		if (!$button.jquery) {
			$button = $($button);
		}
		return $button.attr('id').substring(type.length +1, $button.attr('id').length);
	},
	
	onClickAndShowButton: function(id, callbackFunction, context, arg) {
		var $myButton = getByID(id);
		$myButton.off();
		$myButton.on('click', $.proxy(callbackFunction, context, arg));
		$myButton.show();
	},

	enableFormElements: function( additionSelector='', exceptionList=[]) {
		this.switchFormElements(false, additionSelector, exceptionList);
	},
	
	disableFormElements: function( additionSelector='', exceptionList=[]) {
		this.switchFormElements(true, additionSelector, exceptionList);
	},
	
	switchFormElements: function(enable, additionSelector='', exceptionList=[]) {
		$(additionSelector + ' .form-control').each(function() {
			var $thisElem = $(this);
			if (exceptionList.indexOf($thisElem.attr('id')) == -1) {
				$thisElem.prop("disabled", enable);
			}
		});
	},
	
	show: function(id) {
		var $toShow = getByID(id);
		if ($toShow && $toShow.length > 0) {
			$toShow.show();
		}
	},
	
	hide: function(id) {
		var $toHide = getByID(id);
		if ($toHide && $toHide.length > 0) {
			$toHide.hide();
		}
	}

});

$.pluginMaker(AdminTool.AccessManagement);


$( document ).ready(function() {
	
});
