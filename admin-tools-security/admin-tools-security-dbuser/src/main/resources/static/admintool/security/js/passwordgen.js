AdminTool.PasswordGenerator = function(parent) {
	var self = this;

	this.construct = function(parent) {
		this.parent = parent;
		this.options = {};
		$.extend(true, this.options, parent.options);
	};
	
	this.generatePass = function (plength) {
		
		if (!plength && self.options.hasOwnProperty('passwordLength')) {
			plength = self.options.passwordLength;
		}
		if (!plength) {
			plength = 12;
		}

	    var keylistalpha="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMOPQRSTUVWXYZ";
	    var keylistint="1234567890";
	    var keylistspec="!@#_$.-";
	    var temp='';
	    var len = plength/2;
	    var len = len - 1;
	    var lenspec = plength-len-len;

	    for (i=0;i<len;i++) {
	    	temp+=keylistalpha.charAt(Math.floor(Math.random()*keylistalpha.length));
	    }
	    for (i=0;i<lenspec;i++) {
	    	temp+=keylistspec.charAt(Math.floor(Math.random()*keylistspec.length));
	    }
	    for (i=0;i<len;i++) {
	    	temp+=keylistint.charAt(Math.floor(Math.random()*keylistint.length));
	    }
	    return self.shuffle(temp.split('')).join('');
	};
	
	//https://stackoverflow.com/questions/6274339/how-can-i-shuffle-an-array
	this.shuffle = function(a) {
	    var j, x, i;
	    for (i = a.length - 1; i > 0; i--) {
	        j = Math.floor(Math.random() * (i + 1));
	        x = a[i];
	        a[i] = a[j];
	        a[j] = x;
	    }
	    return a;
	};
	
	this.construct(parent);
};