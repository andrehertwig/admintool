var leveCss = null;
var prefix = 'bg';

AdminTool.Log4j = function(el, options) {
	if (el) {
        this.init(el, options)
    }
}
AdminTool.Log4j.prototype = new AdminTool.Core();

$.extend(AdminTool.Log4j.prototype, {
	
	name : 'adminToolLog4j',
	
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
		
		$('.removeCustomLogger').click(function(){
			sendRequest("/admintool/log4j2/removeCustomLoggers", "POST", "text", function(data) {
				location.reload();
			});
		});
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

$.pluginMaker(AdminTool.Log4j);

$( document ).ready(function() {
	getByID('log4jContent').adminToolLog4j();
});