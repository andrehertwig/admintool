# The Core-Security integration Plugin
> Integrates Spring Security and overrides the menu to use admintool.*.securityRoles to check if menu entries could be shown

## Introduced with
* admin-tools-core:1.0.1

## Requirements, Dependencies
* spring-framework (core, security)


## Usage

```xml

	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-core</artifactId>
		<version>1.0.1</version>
	</dependency>
	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-core-security</artifactId>
		<version>1.0.1</version>
	</dependency>
	
```

To use the login page it could be configured like this:
```java

	http
		.authorizeRequests()
			.antMatchers(AdminTool.ROOTCONTEXT + "/myComponent").hasAnyRole("ADMIN", "USER")
			.antMatchers(AdminTool.ROOTCONTEXT).permitAll()
			.antMatchers(AdminTool.ROOTCONTEXT + "/**").permitAll()
		.and()
			.formLogin()
				.loginPage(AdminTool.ROOTCONTEXT + "/login")
					.defaultSuccessUrl(AdminTool.ROOTCONTEXT, false)
					.permitAll()
		.and()
			.logout()
				.logoutUrl(AdminTool.ROOTCONTEXT + "/logout")
				.logoutSuccessUrl(AdminTool.ROOTCONTEXT)
				.invalidateHttpSession(true)
		
```	

## Configurations

```ini

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