package de.chandre.admintool.security.dbuser.service;

import java.util.Set;

import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.commons.auth.AdminToolUserDetailsService;
import de.chandre.admintool.security.commons.auth.UserTO;
import de.chandre.admintool.security.dbuser.domain.ATUser;

/**
 * extend interface for DB UserDetails service
 * 
 * @author André
 * @since 1.1.7
 */
public interface AdminToolSecDBUserDetailsService extends AdminToolUserDetailsService {

	ATUser getUser(String username);
	
	ATUser getUserForId(String userid);

	ATUser saveUser(ATUser user);
	
	ATUser saveUser(ATUser user, boolean encodePassword);

	ATUser setUserName(ATUser currentUser, String newUserName);

	ATUser setUserLocked(ATUser currentUser, boolean locked);

	ATUser setUserExpired(ATUser currentUser, boolean expired);

	ATUser setUserEnabled(ATUser currentUser, boolean enabled);
	
	ATUser setUserCredentialsExpired(ATUser currentUser, boolean credentialsExpired);

	ATUser addUserGroups(ATUser user, Set<String> groupNames);

	ATUser removeUserGroups(ATUser user, Set<String> groupNames);

	void loginFailed(String username);

	void loginSuccess(String username);

	ATUser addCients(ATUser user, Set<String> clientNames);

	Set<ATError> createUser(UserTO userTO);

	Set<ATError> updateUser(UserTO userTO);

	void removeByName(String username);

}
