package de.chandre.admintool.security.dbuser.auth;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.data.domain.AuditorAware;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

import de.chandre.admintool.security.dbuser.domain.ATUser;

/**
 * convenience builder for some beans
 * 
 * @author André
 * @since 1.1.7
 *
 */
public class AdminToolSecDBBeans {
	
	public static ATUser createSystemUser() {
		return createDummyUser("SYSTEM");
	}
	
	public static ATUser createDummyUser(String name) {
		ATUser systemUser = new ATUser();
		systemUser.setUsername(name);
		systemUser.setEnabled(false);
		systemUser.setAccountNonLocked(false);
		return systemUser;
	}
	
	/**
	 * creates a auditor provider 
	 * 
	 * @param systemUser
	 * @param anonymousUser
	 * @return
	 */
	public static AuditorAware<String> auditorProvider(ATUser systemUser, ATUser anonymousUser) {
		return new AuditorAware<String>() {
			private final Log LOGGER = LogFactory.getLog(AuditorAware.class);
			public String getCurrentAuditor() {
				SecurityContext secCtx = SecurityContextHolder.getContext();
				if (null == secCtx) {
					return systemUser.getUsername();
				}
				Authentication authentication = secCtx.getAuthentication();

				if (authentication == null || !authentication.isAuthenticated()) {
					LOGGER.debug(String.format("using %s user for auditing", systemUser.getUsername()));
					return systemUser.getUsername();
				}
				else if (authentication.isAuthenticated() && !ATUser.class.isAssignableFrom(authentication.getPrincipal().getClass())) {
					LOGGER.debug(String.format("using %s user for auditing", anonymousUser.getUsername()));
					return anonymousUser.getUsername();
				}
				
			    return ((ATUser) authentication.getPrincipal()).getUsername();
			}
		};
	}

}
