
AdminTool.Jmx = function(el, options) {
	if (el) {
        this.init(el, options)
    }
}
AdminTool.Jmx.prototype = new AdminTool.Core();

$.extend(AdminTool.Jmx.prototype, {
	
	name : 'adminToolJmx',
	
	postInit: function() {
		this.initJsTree()
	},
	
	initJsTree: function() {
		
		$('#jmxTree').on("changed.jstree", $.proxy(this.selectNode, this)).jstree({
			'core' : {
				'data' :  {
		            'url' : getWebContext() + '/admintool/jmx/tree',
		            'data' : function (node) {
		                return { 'id' : node.id };
		            }
		        }
			},
			'types' : {
				'server' : {
					'icon' : 'fa fa-server'
				},
				'domain' : {
					'icon' : 'fa fa-folder-o'
				},
				'mbean' : {
					'icon' : 'fa fa-coffee'
				},
				'attributes' : {
					'icon' : 'fa fa-cubes'
				},
				'attribute' : {
					'icon' : 'fa fa-cube'
				},
				'operations' : {
					'icon' : 'fa fa-cogs'
				},
				'operation' : {
					'icon' : 'fa fa-cog'
				},
			},
			"plugins" : [
			    "state", "search", "types", "wholerow"
			]
		});
	},
	
	selectNode: function(e, data) {
		if(data.selected.length) {
			
			var selectedNode = data.instance.get_node(data.selected[0]);
			if (selectedNode.type == 'attributes') {
				
				console.log('The selected node is: ' + selectedNode.text);
				console.log('The selected parent is: ' + selectedNode.parent);
				console.log('The selected parents parent is: ' + data.instance.get_node(selectedNode.parent).parent);
				
				var domain = this.getParent(data, selectedNode.parent);
				var queryData = {
					'mbean' : selectedNode.parent,
					'domain' : domain,
					'server' : this.getParent(data, domain)
				};
				
				console.log(queryData);
				
				this.sendRequest(
					{
						url: getWebContext() + "/admintool/jmx/attributes", 
						requestType:'POST',
						dataType: "json",
						data: JSON.stringify(queryData),
						my: this
					},
					$.proxy(this.viewAttributeList, this));
				
			} else if (selectedNode.type == 'attribute' || selectedNode.type == 'operation') {
				
				console.log('The selected node is: ' + selectedNode.text);
				try {
					var mbeanType = this.getParent(null, selectedNode);
					
					var isAttribute = mbeanType.indexOf("_attributes") !== -1;
					var mbean = this.getParent(data, mbeanType);
					var domain = this.getParent(data, mbean);
					var server = this.getParent(data, domain);
					
					var queryData = {
						'name' : selectedNode.text,
						'mbean' : mbean,
						'domain' : domain,
						'server' : server
					};
					console.log(queryData);
					
					if(isAttribute) {
						this.loadAttribute(queryData);
					} else {
						this.sendRequest(
						{
							url: getWebContext() + "/admintool/jmx/operation", 
							requestType:'POST',
							dataType: "json",
							data: JSON.stringify(queryData),
							my: this
						},
						$.proxy(this.viewOperation, this));
					}
					
				} catch (e) {
					console.log(e)
				}
			}
		}
	},
	
	getParent: function(selectedNode) {
		return selectedNode.parent;
	},
	
	getParent: function(data, currentNode) {
		if (null == data) {
			return currentNode.parent;
		}
		return data.instance.get_node(currentNode).parent;
	},
	
	loadAttribute: function(queryData) {
		this.sendRequest(
		{
			url: getWebContext() + "/admintool/jmx/attribute", 
			requestType:'POST',
			dataType: "json",
			data: JSON.stringify(queryData),
			showModalOnError: true,
			showXHRErrorInModal: true,
			my: this,
		},
		$.proxy(this.viewAttributeList, this));
	},
	
	viewAttributeList: function(data, query) {
		var result = "";
		var orgData = JSON.parse(query.data);
		if (data && data.methods && data.methods.length > 0) {
			
			for(var i=-1, l=data.methods.length; ++i < l;) {
				var method = data.methods[i];
				if (method.value == null) {
					
				} else if (Array.isArray(method.value) || typeof method.value === 'object') {
					data.methods[i].value = JSON.stringify(method.value, null, "\t");
				}
			}
			
			if(data.methods.length == 1) {
				result = Mustache.render(attributeTpl, data.methods[0]);
			} else {
				data["headline"] = orgData.domain + " - " + orgData.mbean;
				result = Mustache.render(attributeListTpl, data);
			}
		}
		$('#jmxView').html(result);
		$('#jmxView').find('#refreshView').on('click', $.proxy(this.loadAttribute, this, orgData));
	},
	
	viewOperation: function(data, query) {
		console.log(data)
		
		var result = "";
		if (data && data.methods && data.methods.length > 0) {
			
			if(data.success != null && data.success === false) {
				result = Mustache.render(opperationFailedTpl, data.methods[0]);
			} else {
				data.methods[0].success = data.success;
				data.methods[0].successMessage = function () {
					return function (text, render) {
						if (this.success && this.success === true) {
							return '<div id="save_success" class="alert alert-success alert-dismissible" role="alert">'+
							'<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>' +
							'Value saved successfully' + 
							'</div>';
						}
						return "";
					}
				}
				result = Mustache.render(opperationsTpl, data.methods[0]);
			}
		}
		$('#jmxView').html(result);
		$('#jmxView').find('#execute').off();
		if (data && (data.success == null || data.success) && data.methods && data.methods.length > 0) {
			$('#jmxView').find('#execute').on('click', $.proxy(
					this.executeOperation, this, data.methods[0], JSON.parse(query.data)));
		}
		
		var msg = $('#jmxView').find('#save_success');
		if (msg && msg.length > 0) {
			window.setTimeout(function() {
				$("#save_success").fadeTo(500, 0).slideUp(500, function(){
					$(this).remove(); 
				});
			}, 3000);
		}
	},
	
	executeOperation: function(operation, queryData) {
		
		if (operation.parameters && operation.parameters.length > 0) {
			var paramList = [];
			for (var i = -1, l = operation.parameters.length; ++i < l;) {
				var param = operation.parameters[i];
				var parameter = {
					"name" : param.name,
					"type" : param.type,
					"newValue" : $('#jmxView').find(getID(param.name)).val()
				};
				paramList.push(parameter);
			}
			queryData["parameters"] = paramList;
		}
		
		this.sendRequest(
		{
			url: getWebContext() + "/admintool/jmx/operation/execute", 
			requestType:'POST',
			dataType: "json",
			data: JSON.stringify(queryData),
			showModalOnError: true,
			showXHRErrorInModal: true,
			my: this,
		},
		$.proxy(this.viewOperation, this));
	}
});

$.pluginMaker(AdminTool.Jmx);

var attributeListTpl =
	'<div class="row">'+
		'<div class="col-xs-10 col-md-11"><h4>{{headline}}</h4></div>'+
		'<div class="col-xs-2 col-md-1"><a class="pull-right" id="refreshView"><i class="fa fa-refresh"></i></a></div>'+
	'</div>'+
	'<div class="table-responsive"><table class="table no-margin table-hover">'+
		'<tbody>' +
		'{{#methods}}' +
			'<tr id="{{name}}">' +
				'<td title="{{description}}">{{name}}</td>' +
				'<td title="{{type}}"><pre>{{value}}</pre></td>' +
			'</tr>' +
		'{{/methods}}' +
	'</tbody></table></div>';
Mustache.parse(attributeListTpl);

var attributeTpl =
	'<div class="row">'+
		'<div class="col-xs-10 col-md-11"><h4>{{name}}</h4></div>'+
		'<div class="col-xs-2 col-md-1"><a class="pull-right" id="refreshView"><i class="fa fa-refresh"></i></a></div>'+
	'</div>'+
	'<div class="table-responsive"><table class="table no-margin table-hover"><tbody>' +
		'<tr id="dec_{{name}}">' +
			'<td>Description</td>' +
			'<td>{{description}}</td>' +
		'</tr>' +
		'<tr id="type_{{name}}">' +
			'<td>Type</td>' +
			'<td>{{type}}</td>' +
		'</tr>' +
	'</tbody></table></div>'+
	'<pre id="val_{{name}}">{{value}}</pre>';
Mustache.parse(attributeTpl);

var opperationsTpl =
	'<h4>{{name}}</h4>'+
	'{{#successMessage}}1{{/successMessage}}' +
	'<div class="table-responsive"><table class="table no-margin table-hover"><tbody>' +
		'<tr id="dec_{{name}}">' +
			'<td>Description</td>' +
			'<td>{{description}}</td>' +
		'</tr>' +
		'<tr id="type_{{name}}">' +
			'<td>Return-Type</td>' +
			'<td>{{type}}</td>' +
		'</tr>' +
		'{{#parameters}}' +
			'<tr>' +
				'<td title="{{description}}">{{name}}</td>' +
				'<td title="{{type}}">'+
					'<input class="form-control" type="text" id="{{name}}" name="{{name}}" placeholder="{{type}}" value=""/>'+
				'</td>' +
			'</tr>' +
		'{{/parameters}}' +
	'</tbody></table></div>'+
	'<div><button type="button" id="execute" class="btn btn-default">Execute</button></div>'
	;
Mustache.parse(opperationsTpl);

var opperationFailedTpl =
	'<h4>{{name}}</h4>'+
	'<div>Operation failed</div>'
	;
Mustache.parse(opperationFailedTpl);

$( document ).ready(function() {
	
	$(".panel-left").resizable({
		handleSelector: ".splitter",
		resizeHeight: false
	});
	
	$('#jmxContent').adminToolJmx();
});

$(function () {
	var to = false;
	$('#jmxTreeSearch').keyup(function () {
		if(to) { clearTimeout(to); }
		to = setTimeout(function () {
			var v = $('#jmxTreeSearch').val();
			$('#jmxTree').jstree(true).search(v);
		}, 250);
	});
});