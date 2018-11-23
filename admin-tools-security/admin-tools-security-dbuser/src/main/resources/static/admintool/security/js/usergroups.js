
AdminTool.UserGroups = function(el, options) {
	if (el) {
        this.init(el, options)
    }
}
AdminTool.UserGroups.prototype = new AdminTool.AccessRelation();

$.extend(AdminTool.UserGroups.prototype, {
	
	name : 'usergroups',
	
	additionalPostInits: function() {
		this.options = $.extend( this.options, {
			getRolesURL : '/admintool/accessmanagement/usergroup/roles',
		});
		
		this.select2Util = new AdminTool.Select2Util(this);
		
		this.initRoles();
	},
	
	initRoles:function() {
		this.sendRequest({
			url: this.options.getRolesURL, 
			requestType: "GET",
			ctx: this,
		}, function(data, query) {
			query.ctx.select2Util.initSelectFormFromData(data, 'roles', 'Select one or more Role', '100%', true, 'default', '#relationDataModal', true);
		});
	},
	
	porstClearRelationForm: function(){
		this.select2Util.clearSelect('roles');
	},
	
	postShowRelation: function($button){
		var roles = $button.parent().parent().find('.roles').data('roles');
		this.select2Util.setValue('roles', roles);
	},
	
	getFormSelectChoice: function(id, required) {
		var $mySelect = getByID(id);
		if ($mySelect && $mySelect.length > 0) {
			var selections = $mySelect.val();
			if ((!selections || selections.length == 0) && required) {
				getByID(id +'Group').addClass('has-error');
				//has-danger
				this.validationUtil.showCustomError('#'+id, 'Relations are required', this.relationDataFormId);
			}
			return selections;
		}
		return getByID(id +'Group').val().split(",");
	},
	
	additionalValidation: function() {
		var roles = this.getFormSelectChoice('roles', true);
		if (null == roles || roles.length == 0) {
			return true
		}
		return false;
	},
	
	addRelationData: function(relationData) {
		var relationNames = this.getFormSelectChoice('roles', true);
		if (null == relationNames || relationNames.length == 0) {
			return;
		}
		relationData["relationNames"] = relationNames;
	}
	
});

$.pluginMaker(AdminTool.UserGroups);


$( document ).ready(function() {
	if ($("#accessrelation").length > 0) {
		$("#accessrelation").usergroups();
	}
});
