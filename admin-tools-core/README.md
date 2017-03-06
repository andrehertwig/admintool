# The Database Browser Plugin
> The core component


## Features
* provides the extendable admin GUI 

## Introduced with
* admin-tools-core:1.0.0

## Requirements, Dependencies
* spring-framework (core, mvc)
* thymeleaf
* adminlte
* font-awesome

## Usage

```xml

	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-core</artifactId>
		<version>1.1.4</version>
	</dependency>
	
```

## Configurations
```ini

	# to deactivate the functionality
	admintool.core.enabled=true
	
	# if CDN for external java script libs should be used
	admintool.core.useCDN=true
	
	# version of adminLTE if CDN is used. for managed webjars this version should be used.
	admintool.core.adminLTE.cdn.version=2.3.11
	
	# relative path to jquery within adminLTE package
	# since 1.1.4
	admintool.core.adminlte.jquery.path=plugins/jQuery/jquery-2.2.3.min.js
	
	# Path (relative to own project or URL to CDN) for jQuery.
	# Setting the jqueryPath is only required if using a different AdminLTE version than the configured one 
	#   and the (AdminLTE's) distributed version of jQuery or name/path has been changed
	# since 1.1.0
	admintool.core.jquery.path=
	
	# version of fontAwsome if CDN is used. for managed webjars this version should be used.
	admintool.core.fontAwsome.cdn.version=4.7.0
	
	# if CDN link https://cdn.jsdelivr.net/webjars/org.webjars.bower/ or https://cdn.jsdelivr.net/webjars/ should be used 
	# since 1.1.4
	admintool.core.fontAwsome.cdn.useBower=true
	
	# version of ionicons
	# since 1.1.4
	admintool.core.ionicons.cdn.version=2.0.1
	
	# if CDN link https://cdn.jsdelivr.net/webjars/org.webjars.bower/ or https://cdn.jsdelivr.net/webjars/ should be used 
	# since 1.1.4
	admintool.core.ionicons.cdn.useBower=true
	
	# if the stacktrace should be shown on error pages
	admintool.core.showStacktraceOnErrorPage=true
	
```
