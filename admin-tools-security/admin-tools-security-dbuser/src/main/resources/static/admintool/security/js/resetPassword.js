
AdminTool.ResetPassword = function(el, options) {
	if (el) {
        this.init(el, options)
    }
}
AdminTool.ResetPassword.prototype = new AdminTool.AccessManagement();

$.extend(AdminTool.ResetPassword.prototype, {
	
	name : 'resetPassword',
	
	postInit: function() {
		
		this.options = $.extend( this.options, {
			passwordLength		: 14
		});
		
		this.validationUtil = new AdminTool.ValidationUtil(this);
		this.formId = '#profileDataForm';
		this.validationUtil.create(this.formId);
		
		this.passwordGen = new AdminTool.PasswordGenerator(this);
		
		this.initButtons();
	},
	
	initButtons : function() {
		var genPwdButton = getByID('generatePassword');
		genPwdButton.off();
		genPwdButton.on('click', $.proxy(this.generatePassword, this, 'password'));
		genPwdButton.parent().show();
		
		var showPwdButton = getByID('showPassword');
		showPwdButton.off();
		showPwdButton.on('click', $.proxy(this.switchPasswordVisibility, this, 'password'));
		showPwdButton.parent().show();
	}
	
});

$.pluginMaker(AdminTool.ResetPassword);


$( document ).ready(function() {
	if ($("#resetPassword").length > 0) {
		$("#resetPassword").resetPassword();
	}
});
