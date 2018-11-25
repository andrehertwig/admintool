package de.chandre.admintool.security.dbuser.service;

import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Service;

import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.commons.auth.UserTO;
import de.chandre.admintool.security.dbuser.Constants.CommunicationProcess;
import de.chandre.admintool.security.dbuser.domain.ATUser;
import de.chandre.admintool.security.dbuser.service.comm.SendException;

/**
 * Wrapper service for authenticated method to not pollute the UserDetailsService
 * @author Andre
 * @since 1.2.0
 *
 */
@Service
public class ATSecDBUserControllerAuthProxyImpl implements ATSecDBUserControllerAuthProxy {
	
	@Autowired
	private AdminToolSecDBUserDetailsService userDetailsService;

	@Override
	@PreAuthorize(value="hasRole(T(de.chandre.admintool.security.dbuser.AdminToolSecDBRoles).ROLE_USERS.getName())")
	public ATUser getUserForId(String userId) {
		return userDetailsService.getUserForId(userId);
	}

	@Override
	@PreAuthorize(value="hasRole(T(de.chandre.admintool.security.dbuser.AdminToolSecDBRoles).ROLE_USERS_ADD.getName())")
	public Set<ATError> createUser(UserTO userTo) throws SendException {
		return userDetailsService.createUser(userTo);
	}

	@Override
	@PreAuthorize(value="hasRole(T(de.chandre.admintool.security.dbuser.AdminToolSecDBRoles).ROLE_USERS_UPDATE.getName())")
	public Set<ATError> updateUser(UserTO userTo) {
		return userDetailsService.updateUser(userTo);
	}

	@Override
	@PreAuthorize(value="hasRole(T(de.chandre.admintool.security.dbuser.AdminToolSecDBRoles).ROLE_USERS_REMOVE.getName())")
	public void removeByName(String username) {
		userDetailsService.removeByName(username);
	}

	@Override
	@PreAuthorize(value="hasRole(T(de.chandre.admintool.security.dbuser.AdminToolSecDBRoles).ROLE_USERS_UPDATE.getName())")
	public void createResetPassword(String username, CommunicationProcess resetPasswordRequestAdmin) throws SendException {
		userDetailsService.createResetPassword(username, resetPasswordRequestAdmin);
	}

	@Override
	@PreAuthorize(value="hasRole(T(de.chandre.admintool.security.dbuser.AdminToolSecDBRoles).ROLE_USERS_UPDATE.getName())")
	public void setUserEnabled(String username, boolean newState) {
		userDetailsService.setUserEnabled(username, newState);
	}

	@Override
	@PreAuthorize(value="hasRole(T(de.chandre.admintool.security.dbuser.AdminToolSecDBRoles).ROLE_USERS_UPDATE.getName())")
	public void setUserCredentialsExpired(String username, boolean newState) {
		userDetailsService.setUserCredentialsExpired(username, newState);
	}

	@Override
	@PreAuthorize(value="hasRole(T(de.chandre.admintool.security.dbuser.AdminToolSecDBRoles).ROLE_USERS_UPDATE.getName())")
	public void setUserExpired(String username, boolean newState) {
		userDetailsService.setUserExpired(username, newState);
	}

	@Override
	@PreAuthorize(value="hasRole(T(de.chandre.admintool.security.dbuser.AdminToolSecDBRoles).ROLE_USERS_UPDATE.getName())")
	public void setUserLocked(String username, boolean newState) {
		userDetailsService.setUserLocked(username, newState);
	}

}
