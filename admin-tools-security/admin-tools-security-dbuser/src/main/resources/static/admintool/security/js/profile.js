
AdminTool.Profile = function(el, options) {
	if (el) {
        this.init(el, options)
    }
}
AdminTool.Profile.prototype = new AdminTool.AccessManagement();

$.extend(AdminTool.Profile.prototype, {
	
	name : 'profile',
	
	postInit: function() {
		
		this.options = $.extend( this.options, {
			updateProfileURL 		: '/admintool/accessmanagement/user/profile/update',
			changePasswordURL 		: '/admintool/accessmanagement/user/profile/password/update',
			resetPasswordURL 		: '/admintool/accessmanagement/user/profile/password/reset'
		});
		
		this.validationUtil = new AdminTool.ValidationUtil(this);
		this.profileDataFormId = '#profileDataForm';
		this.passwordDataFormId = '#passwordDataForm';
		
		this.select2Util = new AdminTool.Select2Util(this);
		
		this.passwordGen = new AdminTool.PasswordGenerator(this);
		
		this.initEditUser();
		this.initResetPassword();
		this.initChangePassword();
	},
	
	/*
	 * change user profile
	 */
	
	initEditUser: function() {
		getByID('editProfile').on('click', $.proxy(this.prepareEditProfile, this));
	},
	
	prepareEditProfile: function() {
		
		this.validationUtil.reloadValidator(this.profileDataFormId);
		
		this.select2Util.initClassicSelect('#locale', '#profileDataModal');
		this.select2Util.initClassicSelect('#timeZone', '#profileDataModal');
		
		getByID('saveProfile').off();
		getByID('saveProfile').on('click', $.proxy(this.saveProfile, this));
		getByID('profileDataModal').modal('show');
	},
	
	saveProfile: function() {
		var hasErrors = this.validationUtil.validate(this.profileDataFormId);
		if (hasErrors) {
			return;
		}
		
		//build request and send
		var userData = {
			"firstName" : getByID('firstName').val(),
			"lastName" 	: getByID('lastName').val(),
			"email" 	: getByID('email').val(),
			"phone" 	: getByID('phone').val(),
			"timeZone" 	: getByID('timeZone').val(),
			"locale" 	: getByID('locale').val(),
		};
		
		this.sendRequest({
			url: this.options.updateProfileURL, 
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
				query.ctx.showErrorModal('Error saving Profile', data);
				query.ctx.validationUtil.showFieldErrorsOnATErrorList(data, query.ctx.profileDataFormId);
			}
		});
	},
	
	/*
	 * Reset Password 
	 */
	
	initResetPassword: function() {
		var respwbtn = getByID('resetPassword');
		if (respwbtn.length > 0) {
			respwbtn.on('click', $.proxy(this.resetPasswordConfirm, this, respwbtn));
		}
	},
	
	resetPasswordConfirm: function(btn) {
		this.showConfirmModal("Reset Password", "Do you really want to reset your password?", this.resetPassword, btn);
	},
	
	resetPassword: function(btn) {
		this.sendRequest({
			url: this.options.resetPasswordURL, 
			requestType: "POST", 
			dataType: "text",
			showModalOnError: true,
			ctx: this
		},
		function(data, query) {
			if (!data || data == 'false') {
				query.ctx.showErrorModal('Error', 'Error creating reset password Request');
			}
		});
	},
	
	/*
	 * Change password 
	 */
	
	initChangePassword: function() {
		var chpwbtn = getByID('changePassword');
		if (chpwbtn.length > 0) {
			chpwbtn.on('click', $.proxy(this.showChangePasswordModal, this));
		}
	},
	
	initPasswordButtons : function() {
		var genPwdButton = getByID('generatePassword');
		genPwdButton.off();
		genPwdButton.on('click', $.proxy(this.generatePassword, this, 'newPassword'));
		genPwdButton.parent().show();
		
		var showPwdButton = getByID('showNewPassword');
		showPwdButton.off();
		showPwdButton.on('click', $.proxy(this.switchPasswordVisibility, this, 'newPassword', function(fieldtype, ctx) {
			//if new password will be shown, we can hide confirm password
			var cfmPwdField = getByID('passwordConfirm');
			if (fieldtype=='text') {
				getByClazz('passwordConfirmRow').hide();
				cfmPwdField.prop('required', false);
			} else {
				getByClazz('passwordConfirmRow').show();
				cfmPwdField.attr('required', 'required');
			}
			ctx.validationUtil.reloadValidator(ctx.passwordDataFormId);
		}));
		showPwdButton.parent().show();
		
		var showPwdButton = getByID('showCurrentPassword');
		showPwdButton.off();
		showPwdButton.on('click', $.proxy(this.switchPasswordVisibility, this, 'currentPassword'));
		showPwdButton.parent().show();
	},
	
	showChangePasswordModal: function() {
		getByID('currentPassword').val("");
		getByID('newPassword').val("");
		getByID('passwordConfirm').val("");
		
		this.initPasswordButtons();
		
		this.validationUtil.reloadValidator(this.passwordDataFormId);
		
		getByID('savePassword').off();
		getByID('savePassword').on('click', $.proxy(this.savePassword, this));
		getByID('passwordDataModal').modal('show');
		getByID('currentPassword').focus();
	},
	
	savePassword: function() {
		var hasErrors = this.validationUtil.validate(this.passwordDataFormId);
		if (hasErrors) {
			return;
		}
		var newPwdField = getByID('newPassword');
		var newPassword = newPwdField.val();
		var passwordConfirm = getByID('passwordConfirm').val();
		if (newPwdField.attr('type') == 'password' && newPassword != passwordConfirm) {
			this.validationUtil.showCustomError('#passwordConfirm', 'Confirmation password is not equals new password', this.passwordDataFormId);
			return;
		} else {
			passwordConfirm = newPassword;
		}
		
		var data = {
			'currentPassword' : getByID('currentPassword').val(),
			'newPassword' : newPassword,
			'passwordConfirm' : passwordConfirm,
		};
		
		this.sendRequest({
			url: this.options.changePasswordURL, 
			requestType: "POST", 
			dataType: "json", 
			data: JSON.stringify(data),
			showModalOnError: true,
			ctx: this
		},
		function(data, query) {
			if (data && Array.isArray(data) && data.length == 0) {
				location.reload();
			} else {
				//show error modal (AdminTool.Core)
				query.ctx.showErrorModal('Error saving User', data);
				query.ctx.validationUtil.showFieldErrorsOnATErrorList(data, query.ctx.passwordDataFormId);
			}
		});
	}
});

$.pluginMaker(AdminTool.Profile);


$( document ).ready(function() {
	if ($("#currentUserProfile").length > 0) {
		$("#currentUserProfile").profile();
	}
});
