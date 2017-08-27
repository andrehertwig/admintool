# The Jminix integration Plugin
> Shows Jminix Console within a IFrame

 ![Preview image](doc/screen_jminix_org.png?raw=true "AdminTool Jminix UI")

## Introduced with
* admin-tools-core:1.0.0

## Requirements, Dependencies

```xml

	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-core</artifactId>
		<version>1.1.5</version>
	</dependency>
	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-jminix</artifactId>
		<version>1.1.5</version>
	</dependency>
	
```

## Configurations
If spring security is enabled and you want to use the change functionality of JMiniX you have to disable CSRF. 
Furthermore you have to set the frame options, because. 

```java

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http
            // allow services without CSRF token
            .csrf().ignoringAntMatchers("/jmx/**").and()
            //set frameOptions to sameOrigin
            .headers().frameOptions().sameOrigin();
    }

```

The following properties are available:

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