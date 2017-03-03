# The Properties visualization Plugin
> Shows Git Properties if in classpath and Spring environment properties

 ![Preview image](doc/screen_propertyBrowser_org.png?raw=true "AdminTool Properties UI")

## Introduced with
* admin-tools-core:1.0.1

## Usage

```xml

	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-core</artifactId>
		<version>1.1.3</version>
	</dependency>
	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-properties</artifactId>
		<version>1.1.3</version>
	</dependency>
	
```

## Configurations

```ini

	#since 1.0.3
	# to deactivate the admin component
	adminTool.properties.enabled=true
	
	# path to git.properties
	admintool.properties.gitPropertiesPath=classpath:git.properties
	
	# encoding o git.properties
	admintool.properties.gitPropertiesEncoding=UTF-8
	
	# for own implementation or requires admin-tools-core-security
	#semi-colon separated list of Spring Security roles like ROLE_ANONYMOUS;ROLE_ADMIN
	admintool.properties.securityRoles=
	
	# integer value. used by default comparator to order components
	admintool.properties.componentPosition=
	
	# since 1.1.4
	# semicolon separated list of additional properties. Spring resource loading will be used.
	# paths should start either with "classpath:/" or "file:/"
	admintool.properties.addPropertiesPaths=
	
```