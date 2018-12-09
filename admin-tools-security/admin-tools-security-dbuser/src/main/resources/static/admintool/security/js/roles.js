
AdminTool.Roles = function(el, options) {
	if (el) {
        this.init(el, options)
    }
}
AdminTool.Roles.prototype = new AdminTool.AccessRelation();

$.extend(AdminTool.Roles.prototype, {
	
	name : 'roles',
	
	additionalPostInits: function() {
		this.options = $.extend( this.options, {
			getUserGroupsURL : '/admintool/accessmanagement/role/{{name}}/usergroups',
		});
		
		this.initAssignedUserGroups();
	},
	
	additionalTablePostInits: function() {
		this.initAssignedUserGroups();
	},
	
	initAssignedUserGroups: function() {
		var ctx = this;
		$(".assignedUserGroups").each(function () {
			$(this).off();
			$(this).on('click', $.proxy(ctx.showUserGroups, ctx, this));
		});
	},
	
	showUserGroups: function(btn) {
		var $button = $(btn);
		var name = this.getIdentifier($button, "userGroups");
		var displayName =  $button.parent().parent().find('.displayName').text();
		 
		this.sendRequest({
			url: Mustache.render(this.options.getUserGroupsURL, {"name": name}), 
			requestType: "GET",
			ctx: this,
			roleName: displayName
		}, function(data, query) {
			query.ctx.renderUserGroupsTable(data, query.roleName);
		});
	},
	
	renderUserGroupsTable: function(data, roleName) {
		
		if (data && Array.isArray(data) && data.length > 0) {
			getByID("assignedUserGroupsModal").modal('show');
			
			getByID('assignedUserGroupsTitleAdd').text(roleName);
			
			if ( $.fn.dataTable.isDataTable( '#assignedUserGroupsTable' ) ) {
				var dataTable = getByID("assignedUserGroupsTable").DataTable();
				dataTable.clear();
				dataTable.rows.add(data);
				dataTable.draw();
			}
			else {
				getByID("assignedUserGroupsTable").DataTable({
					data: data,
					columns: [
				        { data: 'name', title: 'Name'},
				        { data: 'displayName', title: 'Display name'},
				        { data: 'active', title: 'Active'}
				    ]
				});
			}
		} else {
			if ( $.fn.dataTable.isDataTable( '#assignedUserGroupsTable' ) ) {
				var dataTable = getByID("assignedUserGroupsTable").DataTable();
				dataTable.clear();
			}
		}
	},
	
	
});

$.pluginMaker(AdminTool.Roles);


$( document ).ready(function() {
	if ($("#accessrelation").length > 0) {
		$("#accessrelation").roles();
	}
});
