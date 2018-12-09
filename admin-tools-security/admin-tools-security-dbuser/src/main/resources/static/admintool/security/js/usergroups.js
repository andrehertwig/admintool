
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
			getRolesURL : '/admintool/accessmanagement/role/get',
			getUsersURL : '/admintool/accessmanagement/usergroup/{{name}}/users',
		});
		
		this.select2Util = new AdminTool.Select2Util(this);
		
		this.initRoles();
		this.initAssignedUsers();
	},
	
	additionalTablePostInits: function() {
		this.initAssignedUsers();
	},
	
	initRoles:function() {
		this.sendRequest({
			url: this.options.getRolesURL, 
			requestType: "GET",
			ctx: this,
		}, function(data, query) {
			if (data && data.errors && Array.isArray(data.errors) && data.errors.length == 0) {
				//show error modal (AdminTool.Core)
				query.ctx.showErrorModal('Error getting roles', data.errors);
				query.ctx.validationUtil.showFieldErrorsOnATErrorList(data.errors, query.ctx.relationDataFormId);
			} else {
				query.ctx.select2Util.initSelectFormFromData(data, 'roles', 'Select one or more Role', '100%', true, 'default', '#relationDataModal', true);
			}
		});
	},
	
	initAssignedUsers: function() {
		var ctx = this;
		$(".assignedUsers").each(function () {
			$(this).off();
			$(this).on('click', $.proxy(ctx.showUsers, ctx, this));
		});
	},
	
	showUsers: function(btn) {
		var $button = $(btn);
		var name = this.getIdentifier($button, "users");
		var displayName =  $button.parent().parent().find('.displayName').text();
		 
		this.sendRequest({
			url: Mustache.render(this.options.getUsersURL, {"name": name}), 
			requestType: "GET",
			ctx: this,
			userGroupName: displayName
		}, function(data, query) {
			query.ctx.renderUsersTable(data, query.userGroupName);
		});
	},
	
	renderUsersTable: function(data, userGroupName) {
		
		if (data && Array.isArray(data) && data.length > 0) {
			getByID("assignedUsersModal").modal('show');
			getByID('assignedUsersTitleAdd').text(userGroupName);
			
			if ( $.fn.dataTable.isDataTable( '#assignedUsersTable' ) ) {
				var dataTable = getByID("assignedUsersTable").DataTable();
				dataTable.clear();
				dataTable.rows.add(data);
				dataTable.draw();
			}
			else {
				getByID("assignedUsersTable").DataTable({
					data: data,
					columns: [
				        { data: 'username', title: 'User name'},
				        { data: 'firstName', title: 'First name'},
				        { data: 'lastName', title: 'Last name'},
				        { data: 'lastLoginISO', title: 'Last login'}
				    ]
				});
			}
		} else {
			if ( $.fn.dataTable.isDataTable( '#assignedUsersTable' ) ) {
				var dataTable = getByID("assignedUsersTable").DataTable();
				dataTable.clear();
			}
		}
	},
	
	postClearRelationForm: function(){
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
