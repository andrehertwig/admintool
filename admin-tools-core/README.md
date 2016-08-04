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
		<version>1.0.0</version>
	</dependency>
	
```

## Configurations
```ini

	# to deactivate the functionality
	admintool.core.enabled=true
	
	# if CDN for external java script libs should be used
	admintool.core.useCDN=true
	
	# version of adminLTE if CDN is used. for managed webjars this version should be used.
	admintool.core.adminLTE.cdn.version=2.3.3
	
	# version of fontAwsome if CDN is used. for managed webjars this version should be used.
	admintool.core.fontAwsome.cdn.version=4.6.3
	
	# if the stacktrace should be shown on error pages
	admintool.core.showStacktraceOnErrorPage=true
	
```
