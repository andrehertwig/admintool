
AdminTool.Users = function(el, options) {
	if (el) {
        this.init(el, options)
    }
}
AdminTool.Users.prototype = new AdminTool.Core();

$.extend(AdminTool.Users.prototype, {
	
	name : 'users',
	
	postInit: function() {
		$("#users_table").DataTable();
		
		if (!this.userDataFormId) {
			this.userDataFormId = '#userDataForm';
		}
		
		var ctx = this;
		this.userNames = [];
		$(".editUser").each(function () {
			ctx.userNames.push($(this).text());
		});
		this.userRoles = [];
		this.roleNamesSelect = null;
		this.initUserRoles();
		
		this.initStatusChange("enable");
		this.initStatusChange("expire");
		this.initStatusChange("lock");
		this.initStatusChange("expcred");
		
		this.initEditUser();
		
		this.initAddUser();
	},
	
	initUserRoles: function() {
		this.sendRequest({
			url: '/admintool/users/roles', 
			requestType: "GET",
			ctx: this,
		}, function(data, query) {
			query.ctx.initUserForm(data);
		});
	},
	
	getSelectTemplate: function() {
		return '<select id="authorities" multiple="multiple" class="form-control">'+
			'{{#roles}}<option value="{{.}}">{{.}}</option>{{/roles}}'+
		'</select>';
	},
	
	initUserForm: function(roles) {
		if (roles && Array.isArray(roles) && roles.length > 0) {
			this.userRoles = roles;
			getByID('authorities').replaceWith(Mustache.render(this.getSelectTemplate(), {roles: this.userRoles}));
			
			this.roleNamesSelect = getByID('authorities').select2({
				  placeholder: roles[0],
				  width: '100%'
			});
		}
	},
	
	initAddUser: function() {
		$('#addUser').click(function() {
			getByID("#users").users("prepareAddUserForm");
		});
	},

	initStatusChange: function(type) {
		$("." + type).each(function () {
			var button = $(this);
			button.click(function() {
				getByID("#users").users("changeState", type, this);
			});
		});
	},
	
	changeState: function(type, btn) {
		
		var button = $(btn);
		var data = {
			"username" : this.getUserName(button, type),
			"newState" : !(button.text().trim() == 'true')
		}
		
		this.sendRequest({
			url: '/admintool/users/changeState/' + type, 
			requestType: "POST", 
			dataType: "text",
			data: JSON.stringify(data),
			showModalOnError: true,
			ctx: this,
			btn: button,
			newState: data.newState
		},
		function(data, query) {
			if (data && data == 'true') {
				query.btn.text(query.newState);
			}
		});
	},
	
	getUserName: function($button, clazz) {
		return $button.attr('id').substring(clazz.length +1, $button.attr('id').length)
	},
	
	prepareAddUserForm: function() {
		this.clearUserForm();
		this.setRequiredFields(true);
		getByID('saveUser').off();
		getByID('saveUser').on('click', $.proxy(this.saveUser, this, false));
		getByID('userDataModal').modal('show');
	},
	
	initEditUser: function() {
		$(".editUser").each(function () {
			var button = $(this);
			button.click(function() {
				getByID("#users").users("editUser", this);
			});
		});
	},
	
	editUser: function(btn) {
		this.clearUserForm();
		var button = $(btn);
		var username = this.getUserName(button, 'editUser');
		var roles = button.parent().parent().find('.roles').text();
		
		getByID('#userName').val(username);
		getByID('#authorities').val(roles);
		this.setRequiredFields(false);
		
		getByID('saveUser').off();
		getByID('saveUser').on('click', $.proxy(this.saveUser, this, true));
		
		getByID('userDataModal').modal('show');
	},
	
	setRequiredFields: function(required) {
		var checkbox = getByID('#userPasswordOverride');
		var pwd = getByID('#userPassword');
		var username = getByID('#userName')
		
		checkbox.prop('checked', required);
		checkbox.prop('disabled', required);
		if (required) {
			pwd.attr('required', 'required');
		} else {
			pwd.prop('required', required);
		}
		
		username.prop('disabled', !required);
		if (required) {
			username.attr('required', 'required');
		} else {
			username.prop('required', required);
		}
	},
	
	clearUserForm: function() {
		$(this.userDataFormId + ' input').each(function() {
			$(this).val("");
		});
		this.reloadValidator(this.userDataFormId);
	},
	
	getFormAuthorities: function() {
		if (null != this.roleNamesSelect) {
			var roles = this.roleNamesSelect.val();
			if (!roles || roles.length == 0) {
				getByID('authoritiesGroup').addClass('has-error');
				//has-danger
				this.showCustomError('#authorities', 'Authorities are required', this.userDataFormId);
			}
			return roles;
		}
		return getByID('#authorities').val().split(",");
	},
	
	saveUser: function(edit) {
		getByID(this.userDataFormId).validator('validate');
		if (this.hasValidationErrors(this.userDataFormId)) {
			return;
		}
		var username = getByID('#userName').val();
		if(!edit && this.userNames.indexOf(username) != -1) {
			this.showCustomError('#userName', 'Username must be uniqe', this.userDataFormId);
			return;
		}
		var authorities = this.getFormAuthorities();
		if (null == authorities || authorities.length == 0) {
			return;
		}
		
		var userData = {
			"username" : username,
			"authorities" : authorities
		};
		var checkbox = getByID('#userPasswordOverride');
		if (checkbox.is(':checked')) {
			userData["password"] = getByID('#userPassword').val();
		}
		
		var type = "addUser";
		if (edit) {
			type = "updateUser";
		}
		this.sendRequest({
			url: '/admintool/users/' + type, 
			requestType: "POST", 
			dataType: "text", 
			data: JSON.stringify(userData),
			showModalOnError: true,
			ctx: this
		},
		function(data, query) {
			if (data && data == 'true') {
				location.reload();
			} else {
				//show error modal (AdminTool.Core)
				query.ctx.showErrorModal('Error saving User', data);
			}
		});
	},
	
	reloadValidator: function(formId) {
		getByID(formId).validator('destroy');
		//validator doesn't remove everything
		getByID(formId).find('.form-control-feedback').removeClass('glyphicon-remove');
		getByID('authoritiesGroup').removeClass('has-error has-danger');
		getByID(formId).validator();
	},

	hasValidationErrors: function (id) {
		return getByID(id).data('bs.validator').hasErrors();
	},
	
	showCustomError: function (fieldId, message, formId) {
		var $field = getByID(fieldId);
		$field.data('bs.validator.errors', [message]);
		getByID(formId).data('bs.validator').showErrors($field);
	}
	
});

$.pluginMaker(AdminTool.Users);


$( document ).ready(function() {
	if ($("#users").length > 0) {
		$("#users").users();
	}
});
