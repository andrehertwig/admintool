var datasourceNames = null;
var examples = null;
var metaData = {};
var codeMirrors = {};


(function( $, window, document, undefined ) {
	
	var Tab = function(_root, selector, number) {
		this.root = _root;
		this.selector = selector;
		this.number = number;
	    this.$elem = $(selector);
	    
	    this.cm = null;
	    
	    this._init();
	};
	
	Tab.prototype = {
		constructor: Tab,
			
		_init: function() {
			this.initCheckboxes();
			this.initCodeMirror('text/x-sql');
			this.initFunctions();
			setTimeout(function(my){my.showDBInfo();}, 3000, this);
			return this;
		},
		
		initCheckboxes: function() {
			this.$elem.find('input').iCheck({
				checkboxClass: 'icheckbox_minimal',
				radioClass: 'iradio_minimal',
//					increaseArea: '20%' // optional
			});
		},
		
		initFunctions: function() {
			$("#execute_" + this.number).off();
			$("#execute_" + this.number).on({'click': $.proxy(this.root.executeQuery, this)});
			
			$("#examples_" + this.number).off();
			$("#examples_" + this.number).on({'change': $.proxy(this.applyExample, this)});
			
			$("#datasourceName_" + this.number).off();
			$("#datasourceName_" + this.number).on({'change': $.proxy(this.changeDataSource, this)});
		},
		
		initAddTab: function() {
			this.root.executeQuery(this);
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
		
		queryDone: function(data) {
			var $tabContent = $("#tab_" + this.number);
			$tabContent.html(data);
			this.initCheckboxes();
			this.initFunctions();
			this.showDBInfo();
			this.initCodeMirror('text/x-sql');
		},
		
		initCodeMirror: function(mode) {
			if ($("#statement_" + this.number).length > 0) {
				//TODO: set the regarding driver vendor
				this.cm = CodeMirror.fromTextArea(document.getElementById("statement_" + this.number), {
					mode: mode,
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
		}
	};
	
	var DbBrowser = function(elem, settings) {
		this.elem = elem;
	    this.$elem = $(elem);
	    this.options = settings;
	    this.admintool = $('#admintool').data("admintool.root");
	    
	    this.tabs = {};
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
			for (key in this.tabs) {
				newNumber = Math.max(newNumber, key);
			}
			newNumber++;
			this.tabs[newNumber] = new Tab(this, "#tabInclude_"+ newNumber, newNumber);
			
			$('#tabContent').append('<div id="tab_'+newNumber+'" class="tab-pane">');
			$('#tabNavAdd').before('<li><a data-toggle="tab" href="#tab_'+newNumber+'" aria-expanded="true">Tab '+newNumber+'</a></li>');
			
			this.tabs[newNumber].initAddTab();
		},
		
		addTab: function (selector, number) {
			this.tabs[number] = new Tab(this, selector, number);
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
				dataType: "text", requestType: 'POST', my: $tab}, function (result, query) {
					query.my.queryDone(result);
			});
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
