package de.chandre.admintool.core.sec;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

/**
 * Abstract permission handler 
 *  (placed in core to avoid interdependencies)
 * @author Andre
 * @since 1.2.0
 */
public abstract class ATAbstractPermissionHandler {
	
	private Authentication getAuthentication() {
		
		SecurityContext securityContext = SecurityContextHolder.getContext();
		if (securityContext == null)
			return null;

		Authentication authentication = securityContext.getAuthentication();
		if (authentication == null)
			return null;
		return authentication;
	}
	
	protected boolean userHasRole(ATInitRole role) {
		Authentication auth = getAuthentication();
		return auth != null
				? auth.getAuthorities().stream().filter(ga -> ga.getAuthority().equals(AdminToolRoles.PREFIX + role.getName()))
						.findFirst().isPresent()
				: false;
	}
}
