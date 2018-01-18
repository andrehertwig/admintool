package de.chandre.admintool.security.commons.auth;

import java.util.Collection;

import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;

/**
 * Interface for admintool UserDetailsService 
 * @author Andre
 * @since 1.1.5
 */
public interface AdminToolUserDetailsService extends UserDetailsService {
	
	/**
	 * 
	 * @return
	 */
	String getInfoMessage();
	
	/**
	 * 
	 * @param infoMessage
	 */
	void setInfoMessage(String infoMessage);
	
	/**
	 * should return the user details
	 * @return
	 */
	Collection<? extends UserDetails> getUsers();
	
	/**
	 * change the user name
	 * @param currentUsername
	 * @param newUserName
	 */
	void setUserName(String currentUsername, String newUserName);
	
	/**
	 * change the lock state of user
	 * @param username
	 * @param locked set true to lock user
	 */
	void setUserLocked(String username, boolean locked);
	
	/**
	 * change the expired state of user
	 * @param username
	 * @param expired set true to expire user
	 */
	void setUserExpired(String username, boolean expired);
	
	/**
	 * change the enabled state of user
	 * @param username
	 * @param enabled set true to enabled user
	 */
	void setUserEnabled(String username, boolean enabled);
	
	/**
	 * change the credentialsExpired state of user
	 * @param username
	 * @param credentialsExpired set true to set credentialsExpired 
	 */
	void setUserCredentialsExpired(String username, boolean credentialsExpired);

}
