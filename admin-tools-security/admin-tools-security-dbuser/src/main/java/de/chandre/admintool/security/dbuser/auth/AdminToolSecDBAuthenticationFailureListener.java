package de.chandre.admintool.security.dbuser.auth;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.event.AuthenticationFailureBadCredentialsEvent;

import de.chandre.admintool.security.commons.auth.AdminToolAuthenticationFailureListener;
import de.chandre.admintool.security.commons.auth.LoginAttemptService;
import de.chandre.admintool.security.dbuser.service.AdminToolSecDBUserDetailsService;

/**
 * 
 * @author André
 * @since 1.1.7
 *
 */
public class AdminToolSecDBAuthenticationFailureListener extends AdminToolAuthenticationFailureListener {

	@Autowired
	private AdminToolSecDBUserDetailsService userDetailsService;
	
	public AdminToolSecDBAuthenticationFailureListener(LoginAttemptService loginAttemptService) {
		super(loginAttemptService);
	}
	
	@Override
	public void onApplicationEvent(AuthenticationFailureBadCredentialsEvent event) {
		userDetailsService.loginFailed(event.getAuthentication().getName());
		super.onApplicationEvent(event);
	}

}
