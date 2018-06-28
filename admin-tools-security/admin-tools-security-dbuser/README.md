# The Core-Security integration Plugin
> Integrates Spring Security and overrides the menu to use admintool.*.securityRoles to check if menu entries could be shown

## Features
* `simple user view`: a view where you can manage user states, passwords and roles and add new users(since 1.1.5)

![Preview image](doc/screen_userview_org.png?raw=true "AdminTool User-View UI")

## Introduced with
* admin-tools-core:1.1.7

## Requirements, Dependencies
* spring-framework (core, security, spring-data, spring-mvc)
* JPA 2.1



## Usage
Until version 1.1.7 the following dependencies must be used. 
```xml
<dependency>
	<groupId>de.chandre.admin-tools</groupId>
	<artifactId>admin-tools-core</artifactId>
	<version>1.1.7</version>
</dependency>
<dependency>
	<groupId>de.chandre.admin-tools.security</groupId>
	<artifactId>admin-tools-security-dbuser</artifactId>
	<version>1.1.7</version>
</dependency>
```

## Integration
An example configuration can be done analog to file-server demo project.

At first there should be some special beans provided to spring context

### Persistence-Beans
To enable the auditing (creations and last changes) you must provide an AuditProvider. 
```java
@Configuration
@EnableTransactionManagement
@EnableJpaAuditing
@EnableJpaRepositories
public class PersistenceBeans {
	
	@Bean
	public PlatformTransactionManager transactionManager(LocalContainerEntityManagerFactoryBean entityManagerFactory) {
		JpaTransactionManager transactionManager = new JpaTransactionManager();
		transactionManager.setEntityManagerFactory(entityManagerFactory.getObject());
		return transactionManager;
	}
	
	@Bean("systemUser")
	public ATUser systemUser() {
		//creating an system user to catch all job executions for auditing
		return AdminToolSecDBBeans.createSystemUser();
	}
	
	@Bean
	public AuditorAware<String> auditorProvider(@Qualifier("systemUser") ATUser systemUser) {
		//returning the pre-configured AuditProvider
		return AdminToolSecDBBeans.auditorProvider(systemUser, AdminToolSecDBBeans.createDummyUser("UNKNOWN"));
	}
}
```


### Security-Beans
```java
@Configuration
public class SecurityBeans {
	
	@Bean
	public PasswordEncoder passwordEncoder() {
	    return new BCryptPasswordEncoder();
	}
	
	/* 
	 * LoginAttemptService is optional in all services
	 * can also be initialized with maxAttempts: new LoginAttemptServiceImpl(5)
	 */
	@Bean
	public LoginAttemptService loginAttemptService() {
		return new LoginAttemptServiceImpl();
	}
	
	@Bean
	public AdminToolSecDBAuthenticationFailureListener adminToolAuthenticationFailureListener(LoginAttemptService loginAttemptService) {
		return new AdminToolSecDBAuthenticationFailureListener(loginAttemptService);
	}
	
	@Bean("adminToolAuthenticationSuccessHandler")
	public AdminToolSecDBAuthenticationSuccessHandler adminToolAuthenticationSuccessHandler(LoginAttemptService loginAttemptService) {
		return new AdminToolSecDBAuthenticationSuccessHandler(loginAttemptService);
	}

}
```

### Security-Config
This is just an example configuration, role names depending on your own!

```java
@EnableWebSecurity
public class SecurityConfig {

	@Autowired
	public void configureGlobal(AuthenticationManagerBuilder auth, AdminToolSecDBUserDetailsService userDetailsService,
			PasswordEncoder pwEncoder, DataSource dataSource) throws Exception {
		
		auth.userDetailsService(userDetailsService).passwordEncoder(pwEncoder);
	}
	
	@Configuration
	public static class AdminToolSecurityConfig extends WebSecurityConfigurerAdapter {
		
		@Autowired
		private AdminToolSecDBAuthenticationSuccessHandler adminToolAuthenticationSuccessHandler;
		
		public AdminToolSecurityConfig() {
			//optional: disable defaults -> must be set manually in configure() method
			super(true);
		}
		
		@Override
		public void configure(WebSecurity web) throws Exception {
			System.out.println("configure WebSecurity");
		    web
		      .ignoring()
		         .antMatchers("/static/**", "/webjars/**");
		    
		}
		
		@Override
		protected void configure(HttpSecurity http) throws Exception {
			System.out.println("configure HttpSecurity");
			http
				.headers().frameOptions().sameOrigin().and()
				
				.authorizeRequests()
				//Profile settings
					.antMatchers(AdminTool.ROOTCONTEXT + "/accessmanagement/resetpassword/**").permitAll()
					.antMatchers(AdminTool.ROOTCONTEXT + "/accessmanagement/user/profile").fullyAuthenticated()
					.antMatchers(AdminTool.ROOTCONTEXT + "/accessmanagement/user/profile/**").fullyAuthenticated()
				//Accessmanagement
					.antMatchers(AdminTool.ROOTCONTEXT + "/accessmanagement/client").hasAnyRole("CLIENT")
					.antMatchers(AdminTool.ROOTCONTEXT + "/accessmanagement/client/**").hasAnyRole("CLIENT")
					.antMatchers(AdminTool.ROOTCONTEXT + "/accessmanagement/role").hasAnyRole("ROLES")
					.antMatchers(AdminTool.ROOTCONTEXT + "/accessmanagement/role/**").hasAnyRole("ROLES")
					.antMatchers(AdminTool.ROOTCONTEXT + "/accessmanagement/user").hasAnyRole("USERS")
					.antMatchers(AdminTool.ROOTCONTEXT + "/accessmanagement/user/**").hasAnyRole("USERS")
					.antMatchers(AdminTool.ROOTCONTEXT + "/accessmanagement/usergroup").hasAnyRole("GROUPS")
					.antMatchers(AdminTool.ROOTCONTEXT + "/accessmanagement/usergroup/**").hasAnyRole("GROUPS")
					.antMatchers(AdminTool.ROOTCONTEXT + "/accessmanagement").hasAnyRole("ACCMGMT")
					.antMatchers(AdminTool.ROOTCONTEXT + "/accessmanagement/**").hasAnyRole("ACCMGMT")
					
					.antMatchers(AdminTool.ROOTCONTEXT + "/dbbrowser").hasAnyRole("DBBROWSER")
					.antMatchers(AdminTool.ROOTCONTEXT + "/dbbrowser/**").hasAnyRole("DBBROWSER")
					.antMatchers(AdminTool.ROOTCONTEXT + "/filebrowser").hasAnyRole("FILEBROWSER")
					.antMatchers(AdminTool.ROOTCONTEXT + "/filebrowser/**").hasAnyRole("FILEBROWSER")
					.antMatchers(AdminTool.ROOTCONTEXT + "/fileviewer").hasAnyRole("FILEVIEWER")
					.antMatchers(AdminTool.ROOTCONTEXT + "/fileviewer/**").hasAnyRole("FILEVIEWER")
					.antMatchers(AdminTool.ROOTCONTEXT + "/jmx").hasAnyRole("JMX")
					.antMatchers(AdminTool.ROOTCONTEXT + "/jmx/**").hasAnyRole("JMX")
					.antMatchers(AdminTool.ROOTCONTEXT + "/log4j2").hasAnyRole("LOG4J")
					.antMatchers(AdminTool.ROOTCONTEXT + "/log4j2/**").hasAnyRole("LOG4J")
					.antMatchers("/monitoring").hasAnyRole("MELODY")
					.antMatchers(AdminTool.ROOTCONTEXT + "/javamelody").hasAnyRole("MELODY")
					.antMatchers(AdminTool.ROOTCONTEXT + "/javamelody/**").hasAnyRole("MELODY")
					.antMatchers(AdminTool.ROOTCONTEXT + "/properties").hasAnyRole("PROPS")
					.antMatchers(AdminTool.ROOTCONTEXT + "/properties/**").hasAnyRole("PROPS")
					.antMatchers(AdminTool.ROOTCONTEXT + "/quartz").hasAnyRole("QUARTZ")
					.antMatchers(AdminTool.ROOTCONTEXT + "/quartz/**").hasAnyRole("QUARTZ")
					.antMatchers(AdminTool.ROOTCONTEXT, AdminTool.ROOTCONTEXT + "/**").hasAnyRole("GENERIC")
					.anyRequest().anonymous()
				.and()
					.formLogin()
						.loginPage(AdminTool.ROOTCONTEXT + "/login")
						//adding the custom success handler
						.successHandler(adminToolAuthenticationSuccessHandler)
						.permitAll()
				.and()
					.logout()
						.logoutUrl(AdminTool.ROOTCONTEXT + "/logout")
						.logoutSuccessUrl(AdminTool.ROOTCONTEXT)
						.invalidateHttpSession(true)
				.and()
					.exceptionHandling()
						.accessDeniedPage(AdminTool.ROOTCONTEXT + "/content/genericError")
				//configuring defaults
				.and()
					.csrf().and()
					.addFilter(new WebAsyncManagerIntegrationFilter())
					.sessionManagement().and()
					.securityContext().and()
					.requestCache().and()
					.anonymous().and()
					.servletApi().and()
					.apply(new DefaultLoginPageConfigurer<HttpSecurity>())
			;
		}
		
	}
}
```