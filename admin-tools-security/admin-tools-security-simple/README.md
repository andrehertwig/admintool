# The Core-Security integration Plugin
> Integrates Spring Security and overrides the menu to use admintool.*.securityRoles to check if menu entries could be shown

## Features
* `simple user view`: a view where you can manage user states, passwords and roles and add new users(since 1.1.5)

![Preview image](doc/screen_userview_org.png?raw=true "AdminTool User-View UI")

## Introduced with
* admin-tools-core:1.0.1

## Requirements, Dependencies
* spring-framework (core, security)


## Usage
Until version 1.1.6 the following dependencies must be used. 
```xml
<dependency>
	<groupId>de.chandre.admin-tools</groupId>
	<artifactId>admin-tools-core</artifactId>
	<version>1.1.7.3</version>
</dependency>
<dependency>
	<groupId>de.chandre.admin-tools</groupId>
	<artifactId>admin-tools-core-security</artifactId>
	<version>1.1.7.3</version>
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

## Using the User-View

Because managing users in a application is a really specific thing the admin-tools-core-security will only provide abstract classes and a simple view.

There are several things to do get it work. Please check out the [demo project sources](https://github.com/andrehertwig/admintool/tree/develop/admin-tools-demo-core/src/main/java/de/chandre/admintool/security)

you will need:
1. a userDetailService implementing the AdminToolUserDetailsService
2. a controller extending the AbstractAdminToolSecurityViewController
3. a loader (spring component) class extending AbstractAdminToolSecurityViewLoader
