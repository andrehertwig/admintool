package de.chandre.admintool.security.dbuser.service;

import java.util.List;
import java.util.Set;

import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.commons.auth.UserTO;
import de.chandre.admintool.security.dbuser.Constants.CommunicationProcess;
import de.chandre.admintool.security.dbuser.domain.ATUser;
import de.chandre.admintool.security.dbuser.service.comm.SendException;

/**
 * Wrapper service for authenticated method to not pollute the {@link AdminToolSecDBUserDetailsService} which also used by spring 
 * @author Andre
 * @since 1.2.0
 *
 */
public interface ATSecDBUserControllerAuthProxy {

	/**
	 * wraps {@link AdminToolSecDBUserDetailsService#getUserForId(String)}
	 * @param userId
	 * @return
	 */
	ATUser getUserForId(String userId);

	/**
	 * wraps {@link AdminToolSecDBUserDetailsService#createUser(UserTO)}
	 * @param userTo
	 * @return
	 * @throws SendException
	 */
	Set<ATError> createUser(UserTO userTo) throws SendException;

	/**
	 * wraps {@link AdminToolSecDBUserDetailsService#updateUser(UserTO)}
	 * @param userTo
	 * @return
	 */
	Set<ATError> updateUser(UserTO userTo);

	/**
	 * wraps {@link AdminToolSecDBUserDetailsService#removeByName(String)}
	 * @param username
	 */
	void removeByName(String username);

	/**
	 * wraps {@link AdminToolSecDBUserDetailsService#createResetPassword(String, CommunicationProcess)}
	 * @param username
	 * @param resetPasswordRequestAdmin
	 * @throws SendException
	 */
	void createResetPassword(String username, CommunicationProcess resetPasswordRequestAdmin) throws SendException;

	/**
	 * wraps {@link AdminToolSecDBUserDetailsService#setUserEnabled(String, boolean)}
	 * @param username
	 * @param newState
	 */
	void setUserEnabled(String username, boolean newState);

	/**
	 * wraps {@link AdminToolSecDBUserDetailsService#setUserCredentialsExpired(String, boolean)}
	 * @param username
	 * @param newState
	 */
	void setUserCredentialsExpired(String username, boolean newState);

	/**
	 * wraps {@link AdminToolSecDBUserDetailsService#setUserExpired(String, boolean)}
	 * @param username
	 * @param newState
	 */
	void setUserExpired(String username, boolean newState);

	/**
	 * wraps {@link AdminToolSecDBUserDetailsService#setUserLocked(String, boolean)}
	 * @param username
	 * @param newState
	 */
	void setUserLocked(String username, boolean newState);

	List<ATUser> getUsersByUserGroupName(String userGroupId);

}
