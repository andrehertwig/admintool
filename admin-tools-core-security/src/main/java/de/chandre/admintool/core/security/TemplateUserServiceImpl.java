package de.chandre.admintool.core.security;

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
	public String getUserName() {
		SecurityContext securityContext = SecurityContextHolder.getContext();
		if (securityContext == null)
			return null;

		Authentication authentication = securityContext.getAuthentication();
		if (authentication == null)
			return null;

		if (authentication.getAuthorities().size() == 1
				&& authentication.getAuthorities().iterator().next().equals(ROLE_ANONYMOUS)) {
			return "Login";
		}

		return authentication.getName();
	}

	@Override
	public boolean isAnonymous() {
		SecurityContext securityContext = SecurityContextHolder.getContext();
		if (securityContext == null)
			return true;

		Authentication authentication = securityContext.getAuthentication();
		if (authentication == null)
			return true;

		if (authentication instanceof AnonymousAuthenticationToken) {
			return true;
		}
		return false;
	}

	@Override
	public Object getUserDetails() {
		SecurityContext securityContext = SecurityContextHolder.getContext();
		if (securityContext == null)
			return null;

		Authentication authentication = securityContext.getAuthentication();
		if (authentication == null)
			return null;

		if (authentication.getAuthorities().size() == 1
				&& authentication.getAuthorities().iterator().next().equals(ROLE_ANONYMOUS)) {
			return null;
		}

		return authentication.getDetails();
	}

	@Override
	public Object getUserPrincipal() {
		SecurityContext securityContext = SecurityContextHolder.getContext();
		if (securityContext == null)
			return null;

		Authentication authentication = securityContext.getAuthentication();
		if (authentication == null)
			return null;

		if (authentication.getAuthorities().size() == 1
				&& authentication.getAuthorities().iterator().next().equals(ROLE_ANONYMOUS)) {
			return null;
		}

		return authentication.getPrincipal();
	}
}
