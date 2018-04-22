package de.chandre.admintool.security;

import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.WebAuthenticationDetails;

public class AdminToolAuthManagerWrapper implements AuthenticationManager {

	private AuthenticationManager original;
	public AdminToolAuthManagerWrapper(AuthenticationManager original) {
		this.original = original;
	}
	
	@Override
	public Authentication authenticate(Authentication authentication) throws AuthenticationException {
		WebAuthenticationDetails auth = (WebAuthenticationDetails) authentication.getDetails();
		//just for debugging and playing around
		return this.original.authenticate(authentication);
	}

}
