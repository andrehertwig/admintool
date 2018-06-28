AdminTool.Select2Util = function(parent) {
	var self = this;

	this.construct = function(parent) {
		this.parent = parent;
		this.options = {};
		$.extend(true, this.options, parent.options);
	};
	
	this.getSelectMultipleMustacheTemplate = function() {
		return '<select id="{{selectId}}" multiple="multiple" class="form-control">'+
			'{{#result}}'+
				'{{^children}}' +
					'<option value="{{id}}">{{text}}</option>' +
				'{{/children}}'+
				
				'{{#hasChildren}}'+
					'<optgroup label="{{text}}">'+
					'{{#children}}'+
						'<option value="{{id}}">{{text}}</option>' +
					'{{/children}}'+
					'</optgroup>'+
				'{{/hasChildren}}'+
			'{{/result}}'+
		'</select>';
	};
	
	this.clearSelect = function(selectId) {
		self.setValue(selectId);
	};
	
	this.setValue = function(selectId, value = []) {
		if(value && !Array.isArray(value) && value.indexOf(',') != -1) {
			value = value.split(',').map(x => x.trim());
		}
		getByID(selectId).val(value).trigger("change");
	};
	
	this.initClassicSelect = function(selectId, parent=null, placeholder='', width='100%', allowClear=false) {
		return self.initSelect(selectId, parent, placeholder, width, allowClear, "classic");
	};
	
	this.initSelect = function(selectId, parent=null, placeholder='', width='100%', allowClear=false, theme='default') {
		var ddParent = parent;
		if (null == parent) {
			ddParent = $(document.body);
		}
		if (!ddParent.jquery) {
			ddParent = $(parent);
		}
		return getByID(selectId).select2({ 
			width: '100%',
			theme: theme,
			allowClear: allowClear,
			placeholder: placeholder,
			//parent is required for modals where search functionality should work
			dropdownParent: ddParent
		});
	};
	
	/**
	 * initializes a select, by replacing the select with generated template from Mustache
	 * 
	 * @param selectObject the data for select creation
	 * @param selectId id of select element
	 * @param allowClear if option 'allowClear' should be set
	 * @param hideIfNoData if selectObject has no data and this value is true, the closest element of closestSelctor class will be hidden 
	 * @param closestSelctor see hideIfNoData
	 */
	this.initSelectFormFromData = function(selectObject, selectId, placeholder='', width='100%', allowClear=false, theme='default', parent=null,
				hideIfNoData=false, closestSelctor = '.row') {
		if (selectObject && typeof selectObject === "object" && selectObject.result && selectObject.result.length > 0) {
			selectObject.result.hasChildren = function() {return this.children && this.children.length > 0;};
			getByID(selectId).replaceWith(Mustache.render(self.getSelectMultipleMustacheTemplate(), {result: selectObject.result, selectId: selectId}));
			
			self.initSelect(selectId, parent, placeholder, width, allowClear, theme);
		} else if (hideIfNoData) {
			var toHide = getByID(selectId).closest(closestSelctor);
			if(toHide.length > 0) {
				toHide.hide();
			}
		}
	};

	this.construct(parent);
};