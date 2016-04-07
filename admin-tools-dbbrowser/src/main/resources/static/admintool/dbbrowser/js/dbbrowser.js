
(function( $, window, document, undefined ) {
	
	var Tab = function(_root, selector, number) {
		this.root = _root;
		this.selector = selector;
		this.number = number;
	    this.$elem = $(selector);
	    
	    this.cm = null;
	    
	    this.supportedVendors = {
    		'oracle' : 'text/x-plsql', 
    		'mysql'	 : 'text/x-mysql', 
    		'mssql'  : 'text/x-mssql', 
    		'postgre': 'text/x-pgsql', 
            'maria'  : 'text/x-mariadb', 
            'cass'   : 'text/x-cassandra', 
            'hive'   : 'text/x-hive', 
            'default': 'text/x-sql'
	    };
	    
	    this._init();
	};
	
	Tab.prototype = {
		constructor: Tab,
			
		_init: function() {
			this.initCheckboxes();
			this.initCodeMirror();
			this.initFunctions();
			setTimeout(function(my){my.showDBInfo();}, 3000, this);
			return this;
		},
		
		initCheckboxes: function() {
			this.$elem.find('input').iCheck('destroy');
			this.$elem.find('input').iCheck({
				checkboxClass: 'icheckbox_minimal',
				radioClass: 'iradio_minimal',
	//					increaseArea: '20%' // optional
			});
		},
		
		initFunctions: function() {
			this.deactivateBindings();
			$("#execute_" + this.number).on({'click': $.proxy(this.executeQuery, this)});
			$("#examples_" + this.number).on({'change': $.proxy(this.applyExample, this)});
			$("#datasourceName_" + this.number).on({'change': $.proxy(this.changeDataSource, this)});
		},
		
		deactivateBindings: function() {
			$("#execute_" + this.number).off();
			$("#examples_" + this.number).off();
			$("#datasourceName_" + this.number).off();
			$('#removeTab_' + this.number).off();
		},
		
		initAddTab: function() {
			this.executeQuery(this);
		},
		
		removeMe: function() {
			if (this.number == 1) {
				return;
			}
			this.$elem.find('input').iCheck('destroy');
			this.deactivateBindings();
			this.cm.setValue("");
			this.cm.clearHistory();
			this.cm.toTextArea();
			delete this.cm;
			$('#tab_' + this.number).remove();
			$('#tabPane_' + this.number).remove();
			this.root.removeTab(this.number);
		},
		
		changeDataSource: function (event) {
			var datasourceName = $('#datasourceName_' + this.number).val();
			this.showDBInfo(datasourceName);
			var $select = $("#examples_" + this.number);
			$select.html('');
			
			if (this.root.examples.hasOwnProperty(datasourceName)) {
				$select.html('<option class="emptyOption" value="">Choose Example</option>');
				var examples = this.root.examples[datasourceName];
				$.each(examples, function (cluster, example) {
					$select.append($('<optgroup>', { 
						label: cluster
					}));
					for(var i = 0, len= example.length; i < len; i++) {
						$select.append($('<option>', { 
							value: example[i].statement,
							text : example[i].description
						}));
					}
				});
			}
		},
		
		applyExample: function(event) {
			this.cm.getDoc().setValue($(event.target).val());
		},
		
		executeQuery: function(event) {
			var $tab = this;
			if (event instanceof Tab) {
				var $tab = event;
			}
			var query = {
				tab : $tab.number,
				datasourceName : $('#datasourceName_' + $tab.number).val() || $tab.root.datasourceNames[0],
				clobEncoding :  $('#clobEncoding_' + $tab.number).val(),
				showClobs :  $('#showClobs_' + $tab.number).prop("checked"),
				showBlobs :  $('#showBlobs_' + $tab.number).prop("checked"),
				maxResults :  $('#maxResults_' + $tab.number).val() || 100,
				statement :  (null == $tab.cm ? '' : $tab.cm.doc.getValue())
			};
			
			$tab.root.admintool.sendRequest({url: '/admintool/dbbrowser/executeQuery', data: JSON.stringify(query),
				dataType: "text", requestType: 'POST', my: $tab, st: query.statement}, function (result, query) {
					query.my.queryDone(result, query.st);
			});
		},
		
		queryDone: function(data, statement) {
			var $tabContent = $("#tabPane_" + this.number);
			$tabContent.html(data);
			this.$elem = $(this.selector);
			this.initCheckboxes();
			this.initFunctions();
			this.showDBInfo();
			this.initCodeMirror();
			
			this.cm.doc.setValue(statement);
			
			var rem = $('#removeTab_' + this.number);
			if (rem.length > 0) {
				rem.on({'click': $.proxy(this.removeMe, this)})
			}
			
			$('#resultTable_' + this.number).DataTable();
		},
		
		initCodeMirror: function() {
			if ($("#statement_" + this.number).length > 0) {
				//TODO: set the regarding driver vendor
				this.cm = CodeMirror.fromTextArea(document.getElementById("statement_" + this.number), {
					mode: this.supportedVendors['default'],
			        lineNumbers: true,
			        indentWithTabs: true,
			        smartIndent: true,
					styleActiveLine: true,
					matchBrackets: true,
					viewportMargin: Infinity,
					autofocus: true,
					extraKeys: {"Ctrl-Space": "autocomplete"}
				// TODO: hint options
//						hintOptions: {tables: {
//					    	users: {name: null, score: null, birthDate: null},
//					    	countries: {name: null, population: null, size: null}
//					    }}
				});
			}
			setTimeout(function(my){my.setSqlMode();}, 500, this);
		},

		refreshCodeMirror: function () {
			this.cm.refresh();
		},
		
		showDBInfo: function(dsn) {
			var datasourceName = dsn || $('#datasourceName_' + this.number).val();
			if (this.root.metaData.hasOwnProperty(datasourceName)) {
				var myInfo = this.root.metaData[datasourceName];
				if (myInfo !== undefined && null != myInfo) 
					$('#dbInfo_' + this.number).text(myInfo.metadata.driverName + ' | ' + myInfo.metadata.databaseProductVersion);
			}
		},
		
		setSqlMode: function(dsn) {
			var datasourceName = dsn || $('#datasourceName_' + this.number).val();
			var mymime = null; 
			if (this.root.metaData.hasOwnProperty(datasourceName)) {
				var myInfo = this.root.metaData[datasourceName];
				if (myInfo !== undefined && null != myInfo) {
					var driverName = myInfo.metadata.driverName;
					for (var vendorName in this.supportedVendors) {
						var mime = this.supportedVendors[vendorName];
						if(this.contains(driverName, vendorName)) {
							mymime = mime;
							break;
						}
					}
				}
			}
			var info = CodeMirror.findModeByMIME(null != mymime ? mymime : this.supportedVendors['default']);
			//console.log('using: ' + info.mode +  ': ' + info.mime);
			this.cm.setOption("mode", info.mime);
		    CodeMirror.autoLoadMode(this.cm, info.mode);
		    this.refreshCodeMirror();
		},
		
		contains: function(str, search){
			if (str.toLowerCase().indexOf(search) != -1) {
				return true;
			}
			return false
		}
	};
	
	var DbBrowser = function(elem, settings) {
		this.elem = elem;
	    this.$elem = $(elem);
	    this.options = settings;
	    this.admintool = $('#admintool').data("admintool.root");
	    
	    this.tabs = [];
	    this.datasourceNames = null;
	    this.examples = null;
	    this.metaData = {};
	    
	    this._init();
	};
	
	DbBrowser.prototype = {
			
		constructor: DbBrowser,
		
		_init: function() {
			var self = this;
			this.config = $.extend({}, this.defaults, this.options);
			this.$elem.data( "admintool.dbBrowser" , this );
			
			this.admintool.addPlugin("admintool.dbBrowser", this);
			
			this.getStaticObjects();
			
			$('#addTab').on({'click':  $.proxy(this._addNewTab, this)});
			
			return this;
		},
		
		_addNewTab: function(event) {
			var newNumber = -1;
			for(var i = 0, len = this.tabs.length; i < len; i++) {
				newNumber = Math.max(newNumber, this.tabs[i].number);
			}
			newNumber++;
			var newTab = new Tab(this, "#tabInclude_"+ newNumber, newNumber);
			this.tabs.push(newTab); 
			
			$('#tabContent').append('<div id="tabPane_'+newNumber+'" class="tab-pane">');
			$('#tabNavAdd').before('<li id="tab_'+newNumber+'"><a data-toggle="tab" href="#tabPane_'+newNumber+
					'" aria-expanded="true">Tab '+newNumber+'</a></li>');
			
			newTab.initAddTab();
		},
		
		addTab: function (selector, number) {
			this.tabs.push(new Tab(this, selector, number));
		},
		
		removeTab: function(number){
			if (number == 1) {
				return;
			}
			var i = 0;
			for(var len = this.tabs.length; i < len; i++) {
				if (number == this.tabs[i].number) {
					break;
				}
			}
			this.tabs.splice(i, 1);
		},
		
		getStaticObjects: function() {
			if (null == this.datasourceNames) {
				this.admintool.sendRequest({url: '/admintool/dbbrowser/getDatasourceNames', my: this}, function (result, query) {
					query.my.datasourceNames = result;
					query.my.getMetaData();
				});
			}
			if (null == this.examples) {
				this.admintool.sendRequest({url: '/admintool/dbbrowser/getExamples', my: this}, function (result, query) {
					query.my.examples = result;
				});
			}
		},

		getMetaData: function() {
			for (var key in this.datasourceNames) {
				var datasourceName = this.datasourceNames[key];
				if (!this.metaData.hasOwnProperty(datasourceName)) {
					this.admintool.sendRequest({url: '/admintool/dbbrowser/getMetaData/' + datasourceName, my: this, dsn: datasourceName}, function (result, query) {
						query.my.metaData[query.dsn] = result;
					});
				}
			}
		}
	};
	
	$.fn.dbBrowser = function( option ) {
		var args = Array.apply(null, arguments);
	    args.shift();
	    
		return this.each(function () {
			 var $this = $(this),
		        data = $this.data('admintool.dbBrowser'),
		        options = typeof option === 'object' && option;
			 
			 if (!data) {
				 return new DbBrowser(this, options);
			 }
			 if (typeof option === 'string') {
		        data[option].apply(data, args);
		     }
	    });
	};
	
	$.fn.dbBrowser.Constructor = DbBrowser;
	
})(jQuery, window, document);

$( document ).ready(function() {
	$('#admintool').admintool();
	$("#dbBrowser").dbBrowser();
	$("#dbBrowser").dbBrowser('addTab', "#tabInclude_1", 1);
});
