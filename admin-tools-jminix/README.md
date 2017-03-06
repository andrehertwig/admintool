# The Jminix integration Plugin
> Shows Jminix Console within a IFrame

 ![Preview image](doc/screen_jminix_org.png?raw=true "AdminTool Jminix UI")

## Introduced with
* admin-tools-core:1.0.0

## Requirements, Dependencies
If spring security is enabled following should be set:

```java

	http
		.headers().frameOptions().sameOrigin();
		
```	

## Usage

```xml

	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-core</artifactId>
		<version>1.1.4</version>
	</dependency>
	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-jminix</artifactId>
		<version>1.1.4</version>
	</dependency>
	
```

## Configurations

```ini

	#since 1.0.3
	# to deactivate the admin component
	adminTool.jminix.enabled=true

	# relative path to Jminix
	adminTool.jminix.path=/jmx/
	
	#since 1.0.1
	# for own implementation or requires admin-tools-core-security
	#semi-colon separated list of Spring Security roles like ROLE_ANONYMOUS;ROLE_ADMIN
	admintool.jminix.securityRoles=
	
	#since 1.0.1
	# integer value. used by default comparator to order components
	admintool.jminix.componentPosition=
	
```