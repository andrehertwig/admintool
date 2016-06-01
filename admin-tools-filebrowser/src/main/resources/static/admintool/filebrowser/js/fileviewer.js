(function( $, window, document, undefined ) {
	
	var Fileviewer = function(elem, settings) {
		this.elem = elem;
	    this.$elem = $(elem);
	    this.options = settings;
	    this.admintool = $('#admintool').data("admintool.root");
	    this.possibleHints = ['xml','html', 'sql', 'css', 'javascript'];
	    this.cm = null;
	    this.cmVersion = $('#cmVersion').text();
	    //test
	    
	    this._init();
	};
	
	Fileviewer.prototype = {
			
			constructor: Fileviewer,
			
			_init: function() {
				var self = this;
				this.config = $.extend({}, this.defaults, this.options);
				this.$elem.data( "admintool.fileviewer" , this );
				
				this.admintool.addPlugin("admintool.fileviewer", this);
				
				this.initCodeMirror();
				
				return this;
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
			
			
	};
	
	$.fn.fileviewer = function( option ) {
		var args = Array.apply(null, arguments);
	    args.shift();
	    
		return this.each(function () {
			 var $this = $(this),
		        data = $this.data('admintool.fileviewer'),
		        options = typeof option === 'object' && option;
			 
			 if (!data) {
				 return new Fileviewer(this, options);
			 }
			 if (typeof option === 'string') {
		        data[option].apply(data, args);
		     }
	    });
	};
	
	$.fn.fileviewer.Constructor = Fileviewer;
	
})(jQuery, window, document);

$( document ).ready(function() {
	$('#admintool').admintool();
	$("#fileviewer").fileviewer();
});