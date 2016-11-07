var leveCss = null;
var prefix = 'bg';

AdminTool.Log4j = {}

AdminTool.Log4j.Loggers = function(el, options) {
	if (el) {
        this.init(el, options)
    }
}
AdminTool.Log4j.Loggers.prototype = new AdminTool.Core();

$.extend(AdminTool.Log4j.Loggers.prototype, {
	
	name : 'adminToolLog4jLoggers',
	
	postInit: function() {
		this.sendRequest(
			{url: "/admintool/log4j2/getLevelCss/" + prefix, dataType: "json", my: this}, 
			function(data, query) {
				query.my.setLevelCss(data);
			}
		);
		this.bind();
	},
	
	bind : function() {
		if($('#parentLoggers').length > 0) {
			$('#parentLoggers').on('init.dt', $.proxy(this.initEvents, this)).dataTable();
			$('#parentLoggers').on('page.dt', $.proxy(this.initEvents, this));
		}
		var eventsInitialized = false;
		if($('#loggers').length > 0) {
			$('#loggers').on('init.dt', $.proxy(this.initEvents, this)).dataTable();
			$('#loggers').on('page.dt', $.proxy(this.initEvents, this));
			eventsInitialized = true;
		}
		
		if($('.removeCustomLogger').length > 0) {
			$('.removeCustomLogger').click(function(){
				sendRequest("/admintool/log4j2/removeCustomLoggers", "POST", "text", function(data) {
					location.reload();
				});
			});
		}
	},
	
	initEvents : function() {
		$('.changeLogger').each(function() {
			 var $el = $(this);
			 $el.unbind('click');
			 $el.on({'click': function(){
				 getByID('log4jContent').adminToolLog4j('changeLogLevel', $el)}
			 });
		 });
	},
	
	unbind : function() {
		$('.removeCustomLogger').unbind();
		$('.changeLogger').unbind();
	},
	
	
	setLevelCss : function (levels) {
		this.levelCss = levels;
	},
	
	getCurrentLevel: function($link) {
		return $link.html();
	},
	
	getCurrentRow: function($link) {
		return $($link.parent().parent());
	},
	
	changeLogLevel : function(link) {
		var $link = $(link);
		var level = this.getCurrentLevel($link);
		var $tr = this.getCurrentRow($link);
		var name = $tr.find('.logname').text();
		var serviceUrl = "/admintool/log4j2/changeLevel/" + name + "/" + level;
		if ($tr.hasClass('parent')) {
			serviceUrl += "/parent/true";
		}
		
		this.sendRequest(
			{url: serviceUrl, requestType: "POST", dataType: "text", my: this, link: $link}, 
			function(data, query) {
				query.my.changeLogLevelDone(data, query.link);
			}
		);
	},
	
	changeLogLevelDone: function(data, $link) {
		if (data == 'true') {
			var $levelTd = this.getCurrentRow($link).children('.loglevel');
			var oldlevel = $levelTd.text();
			var level = this.getCurrentLevel($link);
			
			$levelTd.removeAddClass(this.levelCss[oldlevel], this.levelCss[level]);
			$levelTd.text(level);
		} else if (data == 'reload') {
			location.reload();
		} else {
			$('#admintoolError').modal();
		}
	}
	
});
$.pluginMaker(AdminTool.Log4j.Loggers);


AdminTool.Log4j.Console = function(el, options) {
	if (el) {
        this.init(el, options)
    }
}
AdminTool.Log4j.Console.prototype = new AdminTool.Core();

$.extend(AdminTool.Log4j.Console.prototype, {
	
	name : 'adminToolLog4jConsole',
	
	postInit: function() {
		this.bind();
		this.count = 0;
	},
	
	bind : function() {
		this.loggerNames = getByID('loggerNames').select2({
			  placeholder: 'Root Logger'
		});
		
		getByID('startConsole').on({'click': $.proxy(this.startConsole, this)});
		getByID('stopConsole').hide();
		getByID('clearConsole').on({'click': $.proxy(this.clearConsole, this)});
		
	},
	
	unbind : function() {
		getByID('loggerNames').unbind();
		getByID('startConsole').unbind();
		getByID('clearConsole').unbind();
	},
	
	startConsole : function() {
		
		var data = {
			name:getByID('name').val(),
			pattern:getByID('pattern').val(),
			encoding:getByID('encoding').val(),
			level:getByID('level').val(),
			loggerNames:this.loggerNames.val()
		};
		
		this.sendRequest(
			{url: "/admintool/log4j2/initConsole", requestType: "POST", dataType: "text", data: JSON.stringify(data), my: this}, 
			function(data, query) {
				query.my.consoleStarted(data);
			}
		);
	},
	
	consoleStarted : function(data) {
		var stopButton = getByID('stopConsole');
		stopButton.on({'click': $.proxy(this.stopConsole, this)});
		stopButton.show();
		this.startUpdateConsole();
	},
	
	startUpdateConsole : function() {
		this.intervalId = setInterval(function() {
			getByID('log4jTail').adminToolLog4jConsole('updateConsole');
		}, 5000);
	},
	
	stopUpdateConsole : function() {
		clearInterval(this.intervalId);
		this.intervalId = null;
	},
	
	updateConsole : function() {
		this.sendRequest(
			{url: "/admintool/log4j2/getConsoleContent", dataType: "text", my: this}, 
			function(data, query) {
				if (null != data && data.trim() != "" && data.trim() != "null") {
					getByID('consoleContent').text(getByID('consoleContent').text() + data);
				}
				query.my.count = query.my.count + 1;
				getByID('count').text(query.my.count);
			}
		);
	},
	
	stopConsole : function() {
		this.stopUpdateConsole();
		this.sendRequest(
			{url: "/admintool/log4j2/stopConsole", dataType: "text", my: this}, 
			function(data, query) {
				query.my.consoleStopped(data);
			}
		);
	},
	
	consoleStopped : function(data) {
		var stopButton = getByID('stopConsole');
		stopButton.unbind();
		stopButton.hide();
	},
	
	clearConsole : function() {
		getByID('count').text("0");
		getByID('consoleContent').text("");
		this.count = 0;
	}

});
$.pluginMaker(AdminTool.Log4j.Console);


$( document ).ready(function() {
	if (getByID('log4jContent').length > 0) {
		getByID('log4jContent').adminToolLog4jLoggers();
	}
	if (getByID('log4jTail').length > 0) {
		getByID('log4jTail').adminToolLog4jConsole();
	}
});