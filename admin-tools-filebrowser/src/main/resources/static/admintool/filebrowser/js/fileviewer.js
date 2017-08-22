
AdminTool.Fileviewer = function(el, options) {
	if (el) {
        this.init(el, options)
    }
}
AdminTool.Fileviewer.prototype = new AdminTool.Core();

$.extend(AdminTool.Fileviewer.prototype, {
	
	name : 'fileviewer',
	
	postInit: function() {
		this.possibleHints = ['xml','html', 'sql', 'css', 'javascript'];
	    this.cm = null;
	    this.cmVersion = $('#cmVersion').text();
	    this.initCodeMirror();
	},
	
	initCodeMirror: function() {
		CodeMirror.modeURL = "../../webjars/codemirror/" + this.cmVersion + "/mode/%N/%N.js";
		this.cm = CodeMirror.fromTextArea(document.getElementById("fileContent"), {
			mode: this.loadModeByExtension(),
	        lineNumbers: true,
	        indentWithTabs: true,
	        smartIndent: true,
			styleActiveLine: true,
			matchBrackets: true,
			viewportMargin: Infinity,
			autofocus: true,
			readOnly: ($('#readOnly').text() == 'true'),
			extraKeys: {"Ctrl-Space": "autocomplete"}
		});
		setTimeout(function(my){my.loadModeByExtension();}, 500, this);
	},
	
	refreshCodeMirror: function () {
		this.cm.refresh();
	},
	
	loadModeByExtension: function() {
		var extension = $('#fileExtension').text();
		var info = CodeMirror.findModeByExtension((null != extension && extension.trim() != '') ? extension : "txt");
		if(!info) {
			info = CodeMirror.findModeByExtension("txt");
		}
		console.log('using: ' + info.mode +  ': ' + info.mime + ', extension is: ' + extension);
		if (null == this.cm) {
			return info.mime;
		}
		this.cm.setOption("mode", info.mime);
		this.loadHints(info);
	    CodeMirror.autoLoadMode(this.cm, info.mode);
	    
	    this.refreshCodeMirror();
	},
	
	loadHints: function(modeInfo) {
		
		if($.inArray(modeInfo.mode, this.possibleHints) > -1) {
			var hintURL = "../../webjars/codemirror/" + this.cmVersion + "/addon/hint/%N-hint.js"
			var filename = hintURL.replace(/%N/g, modeInfo.mode);
			
			var script = document.createElement("script");
			script.setAttribute("type","text/javascript");
			script.setAttribute("src", filename);
			
			if (typeof script!="undefined") {
				var others = document.getElementsByTagName("script")[0];
				others.parentNode.insertBefore(script, others);
			}
		}
	}
});

$.pluginMaker(AdminTool.Fileviewer);

$( document ).ready(function() {
	$("#fileviewer").fileviewer();
});