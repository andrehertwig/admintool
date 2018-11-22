
AdminTool.AccessRelation = function(el, options) {
	if (el) {
        this.init(el, options)
    }
}
AdminTool.AccessRelation.prototype = new AdminTool.AccessManagement();

$.extend(AdminTool.AccessRelation.prototype, {
	
	name : 'accessrelation',
	
	postInit: function() {
		this.type = getByID('relationType').text().trim();
		
		this.options = $.extend( this.options, {
			changeStateURL 		: '/admintool/accessmanagement/{{type}}/state/name/{{name}}',
			updateRelationURL	: '/admintool/accessmanagement/{{type}}/update',
			addRelationURL		: '/admintool/accessmanagement/{{type}}/add',
			removeRelationURL	: '/admintool/accessmanagement/{{type}}/remove/name/{{name}}',
		});
		
		this.gatherAllRelationNames();
		this.validationUtil = new AdminTool.ValidationUtil(this);
		this.relationDataFormId = '#relationDataForm';
		
		
		var dataTable = $("#relation_table").DataTable();
		//init removeables and stoppables after page change
		var ctx = this;
		dataTable.on( 'draw', function () {
		    //console.log( 'Redraw occurred at: '+new Date().getTime() );
			ctx.initStatusChange();
			ctx.initShowRelation();
			ctx.initRemoveRelation();
		});
		
		this.initStatusChange();
		this.initShowRelation();
		this.initAddRelation();
		this.initRemoveRelation();
		
		this.additionalPostInits();
	},
	
	additionalPostInits: function() {},
	
	gatherAllRelationNames: function() {
		this.relationNames = [];
		var ctx = this;
		$(".edit").each(function () {
			ctx.relationNames.push($(this).text());
		});
	},
	
	isNameUnique: function(name, edit) {
		if(!edit && this.relationNames.indexOf(name) != -1) {
			return false;
		}
		return true;
	},
	
	getRelationIdentifier: function($button, type) {
		return this.getIdentifier($button, type);
	},
	
	/* ++++++++++++++++++++
	 *  Change State
	 ++++++++++++++++++++++ */
	
	initStatusChange: function() {
		Mustache.parse(this.options.changeStateURL);
		
		var ctx = this;
		$(".state").each(function () {
			$(this).off();
			$(this).on('click', $.proxy(ctx.changeState, ctx, this));
		});
	},
	
	changeState: function(btn) {
		var button = $(btn);
		var name = this.getRelationIdentifier(button, 'state');
		
		this.sendRequest({
			url: Mustache.render(this.options.changeStateURL, {"type": this.type, "name": name}), 
			requestType: "POST", 
			dataType: "text",
			showModalOnError: true,
			ctx: this,
			btn: button
		},
		function(data, query) {
			if (data && data == 'true') {
				var wasTrue = query.btn.text() === 'true';
				query.btn.text(!wasTrue);
			}
		});
	},
	
	/* ++++++++++++++++++++
	 *  Add/Edit Relation Object
	 ++++++++++++++++++++++ */
	
	isNameEditable: function() {
		return this.type != 'role';
	},
	
	isUserGroup: function() {
		return this.type == 'usergroup';
	},
	
	initAddRelation: function() {
		Mustache.parse(this.options.addRelationURL);
		var $addButton = $('#addRelation');
		if ($addButton && $addButton.length > 0) {
			$addButton.off();
			$addButton.on('click', $.proxy(this.prepareAddRelationForm, this));
		}
	},
	
	porstClearRelationForm: function(){},
	
	clearRelationForm: function() {
		$(this.userDataFormId + ' input').each(function() {
			$(this).val("");
		});
		this.porstClearRelationForm();
		this.validationUtil.reloadValidator(this.relationDataFormId);
	},
	
	prepareAddRelationForm: function() {
		this.clearRelationForm();
		this.editRelation(false);
		
		this.hide('editTitle');
		this.show('createTitle');
		
		getByID('relationDataModal').modal('show');
	},
	
	initShowRelation: function() {
		Mustache.parse(this.options.updateRelationURL);
		var ctx = this;
		$(".edit").each(function () {
			$(this).off();
			$(this).on('click', $.proxy(ctx.showRelation, ctx, this));
		});
	},
	
	showRelation: function(btn) {
		
		this.clearRelationForm();
		
		var $button = $(btn);
		var name = $button.text();
		
		//from parent accessManagement.js
		this.disableFormElements(this.relationDataFormId);
		
		var displayName = $button.parent().parent().find('.displayName').text();
		var description = $button.parent().parent().find('.description').text();
		var stateActive = $button.parent().parent().find('.state').text() === 'true';
		
		getByID('name').val(name);
		getByID('displayName').val(displayName);
		getByID('description').val(description);
		getByID('active').prop('checked', stateActive);
		this.createFancyCheckbox('active');
		
		var $editButton = getByID('editRelationBtn');
		$editButton.off();
		$editButton.on('click', $.proxy(this.editRelation, this, true));
		$editButton.show();
		
		this.hide('saveRelationBtn');
		
		this.show('editTitle');
		this.hide('createTitle');
		
		this.postShowRelation($button);
		
		getByID('relationDataModal').modal('show');
	},
	
	postShowRelation: function($button){},
	
	editRelation: function(existingRelation) {
		
		var exceptionList = [];
		if (!this.isNameEditable()) {
			exceptionList.push('name');
		}
		//from parent accessManagement.js
		this.enableFormElements(this.relationDataFormId, exceptionList);
		
		this.createFancyCheckbox('active');
		
		var $saveButton = getByID('saveRelationBtn');
		$saveButton.off();
		$saveButton.on('click', $.proxy(this.saveRelation, this, existingRelation));
		$saveButton.show();
		
		this.hide('editRelationBtn');
	},
	
	additionalValidation: function() {},
	
	addRelationData: function(relationData) {},
	
	saveRelation: function(existingRelation) {
		
		var hasErrors = this.validationUtil.validate(this.relationDataFormId);
		
		var name = getByID('#name').val();
		if (!this.isNameUnique(name, (existingRelation && this.isNameEditable()))) {
			this.validationUtil.showCustomError('#name', 'Name must be uniqe', this.relationDataFormId);
			return;
		}
		
		if (hasErrors || this.additionalValidation()) {
			return;
		}
		
		//build request and send
		var relationData = {
			"name" 			: name,
			"displayName" 	: getByID('displayName').val(),
			"description" 	: getByID('description').val(),
			"active" 		: getByID('active').is(':checked')
		}
		this.addRelationData(relationData);
		
		var uri = this.options.addRelationURL;
		if (existingRelation) {
			uri = this.options.updateRelationURL;
		}
		
		this.sendRequest({
			url: Mustache.render(uri, {"type": this.type}), 
			requestType: "POST", 
			dataType: "json", 
			data: JSON.stringify(relationData),
			showModalOnError: true,
			ctx: this
		},
		function(data, query) {
			if (data && Array.isArray(data) && data.length == 0) {
				location.reload();
			} else {
				//show error modal (AdminTool.Core)
				query.ctx.showErrorModal('Error saving Relation', data);
				query.ctx.validationUtil.showFieldErrorsOnATErrorList(data, query.ctx.relationDataFormId);
			}
		});
		
	},
	
	createFancyCheckbox: function(id) {
		getByID(id).iCheck('destroy');
		getByID(id).iCheck({
			checkboxClass: 'icheckbox_minimal',
			radioClass: 'iradio_minimal'
//					increaseArea: '20%' // optional
		});
	},
	
	/* ++++++++++++++++++++
	 *  Remove Relation
	 ++++++++++++++++++++++ */
	
	initRemoveRelation: function() {
		var $removeables = $('.remove');
		if ($removeables.length != 0) {
			Mustache.parse(this.options.removeRelationURL);
			var ctx = this;
			$removeables.each(function() {
				$(this).off();
				$(this).on('click', $.proxy(ctx.initRemoveRelationConfirm, ctx, this));
			});
		}
	},
	
	initRemoveRelationConfirm: function(btn) {
		this.showConfirmModal("Remove " + this.type, "Do you really want to delete the "+this.type+": "+this.getRelationIdentifier(btn, 'remove')+"?", this.removeRelation, btn);
	},
	
	removeRelation: function(btn) {
		var button = $(btn);
		var name = this.getRelationIdentifier(button, 'remove');
		
		this.sendRequest({
			url: Mustache.render(this.options.removeRelationURL, {"type": this.type, "name": name}), 
			requestType: "POST", 
			dataType: "text",
			showModalOnError: true,
			ctx: this
		},
		function(data, query) {
			if (data && data == 'true') {
				location.reload();
			} else {
				//show error modal (AdminTool.Core)
				query.ctx.showErrorModal('Error removing Relation', data);
			}
		});
	},

});

$.pluginMaker(AdminTool.AccessRelation);

$( document ).ready(function() {
	if ($("#accessrelation").length > 0 && getByID('relationType').text().trim() != 'usergroup') {
		$("#accessrelation").accessrelation();
	}
});
