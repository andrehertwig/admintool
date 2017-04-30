# The JavaMelody integration Plugin
> Shows JavaMelody within a IFrame

 ![Preview image](doc/screen_melody_org.png?raw=true "AdminTool JavaMelody UI")

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
		<version>1.1.5</version>
	</dependency>
	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-melody</artifactId>
		<version>1.1.5</version>
	</dependency>
	
```

## Configurations

```ini

	#since 1.0.3
	# to deactivate the admin component
	adminTool.melody.enabled=true
	
	# relative path to javaMelody
	adminTool.melody.path=/monitoring
	
	#since 1.0.1
	# for own implementation or requires admin-tools-core-security
	#semi-colon separated list of Spring Security roles like ROLE_ANONYMOUS;ROLE_ADMIN
	admintool.melody.securityRoles=
	
	#since 1.0.1
	# integer value. used by default comparator to order components
	admintool.melody.componentPosition=
	
```