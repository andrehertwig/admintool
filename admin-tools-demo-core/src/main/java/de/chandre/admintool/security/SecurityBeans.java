package de.chandre.admintool.security;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import de.chandre.admintool.core.security.auth.AdminToolAuthenticationFailureListener;
import de.chandre.admintool.core.security.auth.AdminToolAuthenticationSuccessListener;
import de.chandre.admintool.core.security.auth.LoginAttemptService;
import de.chandre.admintool.core.security.auth.LoginAttemptServiceImpl;

@Configuration
public class SecurityBeans {
	
	@Bean
	public LoginAttemptService loginAttemptService() {
		return new LoginAttemptServiceImpl(2);
	}
	
	@Bean
	public AdminToolInMemoryUserDetailsService userDetailsService(LoginAttemptService loginAttemptService) {
		AdminToolInMemoryUserDetailsService uds = new AdminToolInMemoryUserDetailsService();
		uds.setInfoMessage("Because this application is using in-memory authentication, all changes are obsolete after restart.");
		uds.setLoginAttemptService(loginAttemptService);
		return uds; 
	}
	
	@Bean
	public AdminToolAuthenticationFailureListener adminToolAuthenticationFailureListener(LoginAttemptService loginAttemptService) {
		return new AdminToolAuthenticationFailureListener(loginAttemptService);
	}
	
	@Bean
	public AdminToolAuthenticationSuccessListener adminToolAuthenticationSuccessListener(LoginAttemptService loginAttemptService) {
		return new AdminToolAuthenticationSuccessListener(loginAttemptService);
	}
}
