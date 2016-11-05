# The Log4j2 Management Plugin
> Shows all loggers with possibility to change the level 

## Introduced with
* admin-tools-core:1.0.0

## Usage

```xml

	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-core</artifactId>
		<version>1.1.0</version>
	</dependency>
	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-log4j2</artifactId>
		<version>1.1.0</version>
	</dependency>
	
```

## Configurations

```ini
	
	#since 1.0.1
	# for own implementation or requires admin-tools-core-security
	#semi-colon separated list of Spring Security roles like ROLE_ANONYMOUS;ROLE_ADMIN
	admintool.log4j2.securityRoles=
	
	#since 1.0.1
	# integer value. used by default comparator to order components
	admintool.log4j2.componentPosition=
	
```