package de.chandre.admintool.security.commons;

import java.io.Serializable;

import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

/**
 * User service for templates
 * @author Andre
 * @since 1.0.1
 */
@Service("templateUserService")
public class TemplateUserServiceImpl implements TemplateUserService {

	protected static final String ROLE_ANONYMOUS = "ROLE_ANONYMOUS";
	
	@Override
	public Authentication getAuthentication() {
		
		SecurityContext securityContext = SecurityContextHolder.getContext();
		if (securityContext == null)
			return null;

		Authentication authentication = securityContext.getAuthentication();
		if (authentication == null)
			return null;
		return authentication;
	}

	@Override
	public String getUserName() {
		
		Authentication authentication = getAuthentication();
		if (authentication.getAuthorities().size() == 1
				&& authentication.getAuthorities().iterator().next().getAuthority().equals(ROLE_ANONYMOUS)) {
			return "Login";
		}
		return authentication.getName();
	}

	@Override
	public boolean isAnonymous() {
		
		Authentication authentication = getAuthentication();
		if (authentication instanceof AnonymousAuthenticationToken) {
			return true;
		}
		return false;
	}

	@Override
	public Object getUserDetails() {
		
		Authentication authentication = getAuthentication();
		if (authentication.getAuthorities().size() == 1
				&& authentication.getAuthorities().iterator().next().getAuthority().equals(ROLE_ANONYMOUS)) {
			return null;
		}
		return authentication.getDetails();
	}

	@Override
	public Object getUserPrincipal() {
		
		Authentication authentication = getAuthentication();
		if (null == authentication || (authentication.getAuthorities().size() == 1
				&& authentication.getAuthorities().iterator().next().getAuthority().equals(ROLE_ANONYMOUS))) {
			return null;
		}
		return authentication.getPrincipal();
	}
	
	@Override
	public <U extends Serializable> U getUserPrincipal(Class<U> userClass) {
		return userClass.cast(getUserPrincipal());
	}
}
