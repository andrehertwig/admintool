package de.chandre.admintool.security.dbuser.service;

import java.util.Set;

import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.commons.auth.AdminToolUserDetailsService;
import de.chandre.admintool.security.commons.auth.UserTO;
import de.chandre.admintool.security.dbuser.Constants.CommunicationProcess;
import de.chandre.admintool.security.dbuser.domain.ATUser;
import de.chandre.admintool.security.dbuser.service.comm.SendException;

/**
 * extend interface for DB UserDetails service
 * 
 * @author André
 * @since 1.1.7
 */
public interface AdminToolSecDBUserDetailsService extends AdminToolUserDetailsService {

	ATUser getUser(String username);
	
	ATUser getUserForId(String userid);
	
	ATUser getUserForPasswordHash(String passwordHash);

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

	Set<ATError> createUser(UserTO userTO) throws SendException;

	Set<ATError> updateUser(UserTO userTO);

	void removeByName(String username);

	/**
	 * updates the user profile with given information<br>
	 * password, usergroups/authorities and clients will be ignored!
	 * @param userTO
	 * @return
	 */
	Set<ATError> updateProfile(UserTO userTO);

	/**
	 * sets the new password but not changes user states
	 *  
	 * @param username the username
	 * @param password the new password
	 */
	void updatePassword(String username, String password);

	/**
	 * creates an new UUID for passwordLinkHash and saves user
	 * @param username
	 * @param process/user who initializes the request
	 */
	void createResetPassword(String username, CommunicationProcess process) throws SendException;

	/**
	 * sets the new password and enables the user 
	 * @param username the username
	 * @param password the new password
	 */
	void resetPassword(String username, String password);

}
