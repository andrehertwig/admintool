package de.chandre.admintool.security.commons.auth;

import org.springframework.context.ApplicationListener;
import org.springframework.security.authentication.event.AuthenticationSuccessEvent;
import org.springframework.security.web.authentication.WebAuthenticationDetails;

/**
 * listener to listen on good credentials
 * @author Andre
 * @since 1.1.5
 */
public class AdminToolAuthenticationSuccessListener implements ApplicationListener<AuthenticationSuccessEvent> {

	private LoginAttemptService loginAttemptService;
	
	public AdminToolAuthenticationSuccessListener(LoginAttemptService loginAttemptService) {
		this.loginAttemptService = loginAttemptService;
	}

	@Override
	public void onApplicationEvent(AuthenticationSuccessEvent event) {
		if (loginAttemptService.isUseUserName()) {
			loginAttemptService.invalidate(event.getAuthentication().getName());
		}
		if (loginAttemptService.isUseRemoteAddress()) {
			WebAuthenticationDetails auth = (WebAuthenticationDetails) event.getAuthentication().getDetails();
			loginAttemptService.invalidate(auth.getRemoteAddress());
		}
	}
}