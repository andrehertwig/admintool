
AdminTool.Users = function(el, options) {
	if (el) {
        this.init(el, options)
    }
}
AdminTool.Users.prototype = new AdminTool.AccessManagement();

$.extend(AdminTool.Users.prototype, {
	
	name : 'users',
	
	postInit: function() {
		
		this.options = $.extend( this.options, {
			getUserGroupsURL 	: '/admintool/accessmanagement/user/usergroups',
			getClientsURL	 	: '/admintool/accessmanagement/user/clients',
			changeUserStateURL 	: '/admintool/accessmanagement/user/changeState/{{type}}',
			addUserURL 			: '/admintool/accessmanagement/user/add',
			updateUserURL 		: '/admintool/accessmanagement/user/update',
			removeUserURL 		: '/admintool/accessmanagement/user/remove',
			getUserInfoURL 		: '/admintool/accessmanagement/user/get/',
			resetUserPasswordURL: '/admintool/accessmanagement/user/resetPassword',
			passwordLength		: 14
		});

		this.validationUtil = new AdminTool.ValidationUtil(this);
		this.select2Util = new AdminTool.Select2Util(this);
		this.passwordGen = new AdminTool.PasswordGenerator(this);
		
		
		//prepare
		Mustache.parse(this.select2Util.getSelectMultipleMustacheTemplate());
		
		if (!this.userDataFormId) {
			this.userDataFormId = '#userDataForm';
		}
		//init meta data
		var ctx = this;
		this.userNames = [];
		$(".editUser").each(function () {
			ctx.userNames.push($(this).text());
		});
		this.initUserGroups();
		this.initClients();
		this.select2Util.initClassicSelect('locale', '#userDataModal');
		this.select2Util.initClassicSelect('timeZone', '#userDataModal');
		
		var dataTable = $("#users_table").DataTable();
		var ctx = this;
		dataTable.on( 'draw', function () {
		    //console.log( 'Redraw occurred at: '+new Date().getTime() );
			ctx.initStatusChange();
			ctx.initShowUser();
			ctx.initShowUserInfo();
			ctx.initRemoveUser();
			ctx.initResetPassword();
		});
		
		//init functions
		this.initStatusChange();
		this.initShowUser();
		this.initAddUser();
		this.initShowUserInfo();
		this.initRemoveUser();
		this.initResetPassword();
	},
	
	initUserGroups: function() {
		this.sendRequest({
			url: this.options.getUserGroupsURL, 
			requestType: "GET",
			ctx: this,
		}, function(data, query) {
			query.ctx.select2Util.initSelectFormFromData(data, 'authorities', 'Select one or more UserGroup', '100%', true, 'default', '#userDataModal', true);
		});
	},

	initClients: function() {
		this.sendRequest({
			url: this.options.getClientsURL, 
			requestType: "GET",
			ctx: this,
		}, function(data, query) {
			query.ctx.select2Util.initSelectFormFromData(data, 'clients', 'Select one or more Client', '100%', true, 'default', '#userDataModal', true);
		});
	},

	getButtonType: function($button) {
		return $button.attr('id').split('_')[0];
	},
	
	getUserIdentifier: function($button, type) {
		return this.getIdentifier($button, type);
	},
	
	/* ++++++++++++++++++++
	 *  Change User
	 ++++++++++++++++++++++ */

	initStatusChange: function() {
		Mustache.parse(this.options.changeUserStateURL);
		
		var pluginId = this.elementId;
		$(".state").each(function () {
			var button = $(this);
			button.off();
			button.click(function() {
				getByID(pluginId).users("changeState", this);
			});
		});
	},
	
	changeState: function(btn) {
		var button = $(btn);
		var type = this.getButtonType(button);
		var data = {
			"username" : this.getUserIdentifier(button, type),
			"newState" : !(button.text().trim() == 'true')
		}
		
		this.sendRequest({
			url: Mustache.render(this.options.changeUserStateURL, {"type": type}), 
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
			} else if (data && data == 'reload') {
				query.ctx.reloadPage();
			}
		});
	},
	
	/* ++++++++++++++++++++
	 * Reset password
	 * ++++++++++++++++++++++ */
	
	initResetPassword: function() {
		var $resetables = $('.resetPassword');
		if ($resetables.length != 0) {
			var pluginId = this.elementId;
			$resetables.each(function() {
				var $el = $(this);
				$el.off();
				$el.click(function() {
					getByID(pluginId).users("initResetPasswordConfirm", this);
				});
			});
		}
	},
	
	initResetPasswordConfirm: function(btn) {
		this.showConfirmModal("Reset Password", "Do you really want to reset the users password: " + this.getUserIdentifier(btn, 'resetPassword')+"?", this.resetPassword, btn);
	},
	
	resetPassword: function(btn) {
		var button = $(btn);
		var userData = {
			"username" : this.getUserIdentifier(button, 'resetPassword')
		};
		
		this.sendRequest({
			url: this.options.resetUserPasswordURL, 
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
				query.ctx.showErrorModal('Error', 'Error reseting password');
			}
		});
	},
	
	/* ++++++++++++++++++++
	 *  Add/Edit User
	 ++++++++++++++++++++++ */
	
	initAddUser: function() {
		$('#addUser').off();
		$('#addUser').on('click', $.proxy(this.prepareAddUserForm, this));
	},
	
	clearUserForm: function() {
		$(this.userDataFormId + ' input').each(function() {
			$(this).val("");
		});
		
		this.select2Util.clearSelect('authorities');
		this.select2Util.clearSelect('clients');
		this.select2Util.clearSelect('locale');
		this.select2Util.clearSelect('timeZone');
		
		this.reloadValidator(this.userDataFormId);
	},
	
	prepareAddUserForm: function() {
		this.clearUserForm();
		this.editUser(false);
		getByID('editUser').hide();
		getByID('editUserTitle').hide();
		getByID('createUserTitle').show();
		
		this.select2Util.setValue('timeZone', getByID('defaultTimeZone').text());
		this.select2Util.setValue('locale', getByID('defaultLocale').text());
		
		getByID('userDataModal').modal('show');
	},
	
	initShowUser: function() {
		var pluginId = this.elementId;
		$(".showUser").each(function () {
			$(this).off();
			$(this).click(function() {
				getByID(pluginId).users("showUser", this);
			});
		});
	},
	
	showUser: function(btn) {
		this.clearUserForm();
		var $button = $(btn);
		var username = $button.text();
		getByID('username').val(username);
		
		getByID('firstName').val($button.data('firstname'));
		getByID('lastName').val($button.data('lastname'));
		getByID('email').val($button.data('email'));
		getByID('phone').val($button.data('phone'));
		
		this.select2Util.setValue('timeZone', $button.data('timezone'));
		this.select2Util.setValue('locale', $button.data('locale'));
		
		var userGroups = $button.parent().parent().find('.userGroups').data('usergroups');
		this.select2Util.setValue('authorities', userGroups);
		
		var clients = $button.parent().parent().find('.clients').data('clients');
		this.select2Util.setValue('clients', clients);
		
		//from parent accessManagement.js
		this.disableFormElements(this.userDataFormId);
		
		var editUserButton = getByID('editUser');
		editUserButton.off();
		editUserButton.on('click', $.proxy(this.editUser, this, true));
		editUserButton.show();
		getByID('saveUser').hide();
		
		getByID('generatePassword').parent().hide();
		getByID('showPassword').parent().hide();
		
		getByID('editUserTitle').show();
		getByID('createUserTitle').hide();
		
		getByID('userDataModal').modal('show');
	},
	
	editUser: function(existingUser) {
		var saveUserButton = getByID('saveUser');
		saveUserButton.off();
		saveUserButton.on('click', $.proxy(this.saveUser, this, existingUser));
		saveUserButton.show();
		getByID('editUser').hide();
		
		//from parent accessManagement.js
		this.enableFormElements(this.userDataFormId);
		
		this.setRequiredFields(!existingUser);
		
		var genPwdButton = getByID('generatePassword');
		genPwdButton.off();
		genPwdButton.on('click', $.proxy(this.generatePassword, this, 'userPassword'));
		genPwdButton.parent().show();
		
		var showPwdButton = getByID('showPassword');
		showPwdButton.off();
		showPwdButton.on('click', $.proxy(this.switchPasswordVisibility, this, 'userPassword'));
		showPwdButton.parent().show();
	},
	
	setRequiredFields: function(required) {
		var checkbox = getByID('#userPasswordOverride');
		var pwd = getByID('#userPassword');
		var username = getByID('#username')
		
		if (checkbox.length > 0) {
			checkbox.prop('checked', required);
			checkbox.prop('disabled', required);
			if (required) {
				pwd.attr('required', 'required');
			} else {
				pwd.prop('required', required);
			}
		}
		
		username.prop('disabled', !required);
		if (required) {
			username.attr('required', 'required');
		} else {
			username.prop('required', required);
		}
	},
	
	getFormSelectChoice: function(id, required) {
		var $mySelect = getByID(id);
		if ($mySelect && $mySelect.length > 0) {
			var selections = $mySelect.val();
			if ((!selections || selections.length == 0) && required) {
				getByID(id +'Group').addClass('has-error');
				//has-danger
				this.validationUtil.showCustomError('#'+id, 'Relations are required', this.userDataFormId);
			}
			return selections;
		}
		return getByID(id).val().split(",");
	},
	
	saveUser: function(edit) {
		//validate
		
		var hasErrors = this.validationUtil.validate(this.userDataFormId);
		
		var authorities = this.getFormSelectChoice('authorities', true);
		if (null == authorities || authorities.length == 0) {
			hasErrors = true;
		}
		if (hasErrors) {
			return;
		}
		
		var username = getByID('#username').val();
		if(!edit && this.userNames.indexOf(username) != -1) {
			this.validationUtil.showCustomError('#username', 'Username must be uniqe', this.userDataFormId);
			return;
		}
		//build request and send
		var userData = {
			"username" 	: username,
			"firstName" : getByID('firstName').val(),
			"lastName" 	: getByID('lastName').val(),
			"email" 	: getByID('email').val(),
			"phone" 	: getByID('phone').val(),
			"timeZone" 	: getByID('timeZone').val(),
			"locale" 	: getByID('locale').val(),
			"authorities" : authorities,
			"clients" 	: this.getFormSelectChoice('clients', false)
		};
		var checkbox = getByID('#userPasswordOverride');
		if (checkbox.length > 0 && checkbox.is(':checked')) {
			userData["password"] = getByID('#userPassword').val();
		}
		
		var uri = this.options.addUserURL;
		if (edit) {
			uri = this.options.updateUserURL;
		}
		this.sendRequest({
			url: uri, 
			requestType: "POST", 
			dataType: "json", 
			data: JSON.stringify(userData),
			showModalOnError: true,
			ctx: this
		},
		function(data, query) {
			if (data && Array.isArray(data) && data.length == 0) {
				location.reload();
			} else {
				//show error modal (AdminTool.Core)
				query.ctx.showErrorModal('Error saving User', data);
				query.ctx.validationUtil.showFieldErrorsOnATErrorList(data, query.ctx.userDataFormId);
			}
		});
	},
	
	reloadValidator: function(formId) {
		getByID('authoritiesGroup').removeClass('has-error has-danger');
		this.validationUtil.reloadValidator(formId);
	},
	
	generatePass: function (plength) {

	    var keylistalpha="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMOPQRSTUVWXYZ";
	    var keylistint="1234567890";
	    var keylistspec="!@#_$.-";
	    var temp='';
	    var len = plength/2;
	    var len = len - 1;
	    var lenspec = plength-len-len;

	    for (i=0;i<len;i++)
	        temp+=keylistalpha.charAt(Math.floor(Math.random()*keylistalpha.length));

	    for (i=0;i<lenspec;i++)
	        temp+=keylistspec.charAt(Math.floor(Math.random()*keylistspec.length));

	    for (i=0;i<len;i++)
	        temp+=keylistint.charAt(Math.floor(Math.random()*keylistint.length));

	        temp=temp.split('').sort(function(){return 0.5-Math.random()}).join('');

	    return temp;
	},
	
	
	/* ++++++++++++++++++++
	 *  Remove User
	 ++++++++++++++++++++++ */
	
	initRemoveUser: function() {
		var $removeables = $('.remove');
		if ($removeables.length != 0) {
			var pluginId = this.elementId;
			$removeables.each(function() {
				var $el = $(this);
				$el.off();
				$el.click(function() {
					getByID(pluginId).users("initRemoveUserConfirm", this);
				});
			});
		}
	},
	
	initRemoveUserConfirm: function(btn) {
		this.showConfirmModal("Remove User", "Do you really want to delete the user: "+this.getUserIdentifier(btn, 'remove')+"?", this.removeUser, btn);
	},
	
	removeUser: function(btn) {
		var button = $(btn);
		var userData = {
			"username" : this.getUserIdentifier(button, 'remove')
		};
		
		this.sendRequest({
			url: this.options.removeUserURL, 
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
				query.ctx.showErrorModal('Error removing User', data);
			}
		});
	},
	
	/* ++++++++++++++++++++
	 *  Show detailed user Info
	 ++++++++++++++++++++++ */
	
	initShowUserInfo: function() {
		var $infoBtns = $('.info');
		if ($infoBtns.length != 0) {
			var pluginId = this.elementId;
			$infoBtns.each(function() {
				var $el = $(this);
				$el.off();
				$el.click(function() {
					getByID(pluginId).users("showUserInfo", this);
				});
			});
		}
	},
	
	showUserInfo: function(btn) {
		var $btn = $(btn);
		
		this.sendRequest({
			url: this.options.getUserInfoURL + this.getUserIdentifier($btn, 'info'), 
			requestType: "GET", 
			dataType: "json",
			showModalOnError: true,
			ctx: this
		},
		function(data, query) {
			if (data && typeof data === 'object') {
				query.ctx.showUserInfoModal(data);
			} else {
				//show error modal (AdminTool.Core)
				query.ctx.showErrorModal('Error fetching User', data);
			}
		});
	},
	
	showUserInfoModal: function(data) {
		
		this.setInfoField('username', data.username);
		this.setInfoField('lastLogin', data.lastLoginISO);
		this.setInfoField('passwordLastChange', data.passwordDateISO);
		this.setInfoFieldNoBrackets('first_last_name', data.firstName, data.lastName);
		this.setInfoFieldNoBrackets('email_phone', data.email, data.phone, " / ");
		this.setInfoFieldNoBrackets('locale_timeZone', data.locale, data.timeZone, " / ");
		this.setInfoField('userGroups', data.authorities ? data.authorities.join(", ") : "");
		this.setInfoField('roles', data.activeRoles ? data.activeRoles.join(", ") : "");
		this.setInfoField('clients', data.clients ? data.clients.join(", ") : "");
		this.setInfoField('accountExpired', data.accountExpired, data.accountExpiredSinceISO);
		this.setInfoField('accountLocked', data.accountLocked, data.accountLockedSinceISO);
		this.setInfoField('credExpired', data.credentialsExpired, data.credentialsExpiredSinceISO);
		this.setInfoField('loginAttempts', data.loginAttempts, data.lastLoginAttemptISO);
		this.setInfoField('created', data.createdBy, data.createdISO);
		this.setInfoField('modified', data.modifiedBy, data.modifiedISO);
		
		getByID('userInfoModal').modal('show');
	},
	
	setInfoField: function(cellId, value, value2=null) {
		var value2Show = this.normalizeValue(value);
		if (null != value2) {
			value2Show = this.normalizeValue(value) + " (" + value2 + ")";
		}
		if (Array.isArray(value)) {
			value2Show = value.join(',')
		}
		getByID("info_" + cellId).text(value2Show);
	},
	
	setInfoFieldNoBrackets: function(cellId, value, value2, delimiter=" ") {
		getByID("info_" + cellId).text(this.normalizeValue(value) + delimiter + this.normalizeValue(value2));
	},
	
	normalizeValue: function(value) {
		if (null == value || "null" == value) {
			value = "";
		}
		return value;
	}
	
});

$.pluginMaker(AdminTool.Users);


$( document ).ready(function() {
	if ($("#users").length > 0) {
		$("#users").users();
	}
});
