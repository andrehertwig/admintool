package de.chandre.admintool.security.commons.auth;

import org.springframework.context.ApplicationListener;
import org.springframework.security.authentication.event.AuthenticationFailureBadCredentialsEvent;
import org.springframework.security.web.authentication.WebAuthenticationDetails;

/**
 * listener to listen on bad credentials
 * @author Andre
 * @since 1.1.5
 */
public class AdminToolAuthenticationFailureListener implements ApplicationListener<AuthenticationFailureBadCredentialsEvent> {
	
	private LoginAttemptService loginAttemptService;
	
	public AdminToolAuthenticationFailureListener(LoginAttemptService loginAttemptService) {
		this.loginAttemptService = loginAttemptService;
	}

	public void onApplicationEvent(AuthenticationFailureBadCredentialsEvent event) {
		if (loginAttemptService.isUseUserName()) {
			loginAttemptService.loginFailed(event.getAuthentication().getName());
		}
		if (loginAttemptService.isUseRemoteAddress()) {
			WebAuthenticationDetails auth = (WebAuthenticationDetails) event.getAuthentication().getDetails();
			loginAttemptService.loginFailed(auth.getRemoteAddress());
		}
	}
}