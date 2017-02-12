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
		this.requestCount = 0;
		this.lineCount = 0;
		this.maxListLength = 100;
		this.initCheckboxes();
	},
	
	bind : function() {
		this.loggerNames = getByID('loggerNames').select2({
			  placeholder: 'Root Logger'
		});
		
		getByID('startConsole').on({'click': $.proxy(this.startConsole, this)});
		getByClazz('stopConsole').hide();
		getByID('clearConsole').on({'click': $.proxy(this.clearConsole, this)});
		
	},
	
	unbind : function() {
		getByID('loggerNames').unbind();
		getByID('startConsole').unbind();
		getByID('clearConsole').unbind();
	},
	
	initCheckboxes: function() {
		getByID('recursive').iCheck('destroy');
		getByID('recursive').iCheck({
			checkboxClass: 'icheckbox_minimal',
			radioClass: 'iradio_minimal'
//					increaseArea: '20%' // optional
		});
		getByID('overrideLogLevel').iCheck('destroy');
		getByID('overrideLogLevel').iCheck({
			checkboxClass: 'icheckbox_minimal',
			radioClass: 'iradio_minimal'
//					increaseArea: '20%' // optional
		});
	},
	
	startConsole : function() {
		
		var data = {
//			name:getByID('name').val(),
			pattern: getByID('pattern').val(),
			encoding: getByID('encoding').val(),
			recursive: getByID('recursive').prop("checked"),
			overrideLogLevel: getByID('overrideLogLevel').prop("checked"),
			level: getByID('level').val(),
			loggerNames: this.loggerNames.val()
		};
		
		this.sendRequest(
			{
				url: "/admintool/log4j2/initConsole",
				requestType: "POST",
				dataType: "text",
				data: JSON.stringify(data),
				showModalOnError: true,
				my: this
			},
			function(data, query) {
				query.my.consoleStarted(data);
			}
		);
	},
	
	consoleStarted : function(data) {
		var stopButtons = getByClazz('stopConsole');
		stopButtons.on({'click': $.proxy(this.stopConsole, this)});
		stopButtons.show();
		this.startUpdateConsole();
		$.AdminLTE.boxWidget.collapse(getByID('log4jTailConfigCollapse'));
		getByID('startConsole').prop('disabled', true);
	},
	
	startUpdateConsole : function() {
		this.intervalId = setInterval(function() {
			getByID('log4jTail').adminToolLog4jConsole('updateConsole');
		}, 5000);
	},
	
	stopUpdateConsole : function() {
		clearInterval(this.intervalId);
		this.intervalId = null;
		getByID('startConsole').prop('disabled', false);
	},
	
	updateConsole: function() {
		this.sendRequest(
			{url: "/admintool/log4j2/getConsoleContent", dataType: "text", my: this}, 
			function(data, query) {
				if (null != data && data.trim() != "" && data.trim() != "null") {
					var lines = data.trim().split("\n");
					if (lines.length > 0) {
						var existingLines = $('#consoleContent span');
						var existingNumbers = $('#lineNumbers span');
						var hasContent = existingLines.length > 0;
						var clazz;
						for (var i=-1, l=lines.length; ++i < l;) {
							var line = lines[i];
							if (line == null || line == undefined || line.trim() == "") {
								continue;
							}
							clazz = query.my.getSpanClass(line) || clazz;
							
							if (!hasContent) {
								getByID('consoleContent').html(query.my.getText(line, clazz));
								getByID('lineNumbers').html(query.my.getText("1", "text-muted"));
								query.my.lineCount = 1;
							} else {
								$('#consoleContent span:last-child').after(query.my.getText(line, clazz));
								query.my.lineCount++
								$('#lineNumbers span:last-child').after(query.my.getText(query.my.lineCount.toString(), "text-muted"));
							}
						}
						
						var oversize = lines.length + existingLines.length - query.my.getMaxListLength();
						if (oversize > 0) {
							for (var i=-1; ++i < oversize;) {
								$(existingLines[i]).remove();
								$(existingNumbers[i]).remove();
							}
						}
						
						if (getByID('scrollToBottom').prop( "checked" )) {
							$('html, body').scrollTop(getByID('consoleContent')[0].scrollHeight);
						}
					}
				}
				query.my.requestCount++;
				getByID('requestCount').text(query.my.requestCount);
			}
		);
	},
	
	getMaxListLength() {
		var userMaxLength = parseInt(getByID('maxListLength').val());
		if (isNaN(userMaxLength) || userMaxLength == Infinity) {
			return this.maxListLength;
		}
		return userMaxLength;
	},
	
	getText: function(line, clazz) {
		line = line.replace(/</g, '&lt;').replace(/>/g, '&gt;');
		if (undefined !== clazz) {
			return '<span class="logline ' + clazz + '">' + line + '</span>';
		}
		return '<span class="logline">' + line + '</span>';
	},
	
	getSpanClass: function(line) {
		var lowerLine = line.toLowerCase();
		if (lowerLine.indexOf(' error ') != -1 || lowerLine.indexOf(' fatal ') != -1) {
			return "text-danger";
		}
		else if (lowerLine.indexOf(' warn ') != -1) {
			return "text-warning";
		}
		else if (lowerLine.indexOf(' info ') != -1 || lowerLine.indexOf(' debug ') != -1 || lowerLine.indexOf(' trace ') != -1) {
			return "text-info";
		}
		else if (lowerLine.indexOf(' off ') != -1) {
			return "text-muted";
		}
		return undefined;
	},
	
	stopConsole : function() {
		this.stopUpdateConsole();
		this.sendRequest(
			{
				url: "/admintool/log4j2/stopConsole",
				dataType: "text",
				showModalOnError: true,
				my: this
			}, 
			function(data, query) {
				query.my.consoleStopped(data);
			}
		);
	},
	
	consoleStopped : function(data) {
		var stopButtons = getByClazz('stopConsole');
		stopButtons.unbind();
		stopButtons.hide();
	},
	
	clearConsole : function() {
		this.requestCount = 0;
		this.lineCount = 0;
		getByID('requestCount').text(this.requestCount);
		getByID('consoleContent').text("");
		getByID('lineNumbers').text("");
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