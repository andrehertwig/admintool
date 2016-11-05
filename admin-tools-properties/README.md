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
		<version>1.1.0</version>
	</dependency>
	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-properties</artifactId>
		<version>1.1.0</version>
	</dependency>
	
```

## Configurations

```ini

	# path to git.properties
	admintool.properties.gitPropertiesPath=classpath:git.properties
	
	# encoding o git.properties
	admintool.properties.gitPropertiesEncoding=UTF-8
	
	# for own implementation or requires admin-tools-core-security
	#semi-colon separated list of Spring Security roles like ROLE_ANONYMOUS;ROLE_ADMIN
	admintool.properties.securityRoles=
	
	# integer value. used by default comparator to order components
	admintool.properties.componentPosition=
	
```