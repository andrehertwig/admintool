package de.chandre.admintool.core.security.auth;

/**
 * Interface for the login attempt service
 * 
 * @author Andre
 * @since 1.1.5
 */
public interface LoginAttemptService {

	/**
	 * resets the user attempts
	 * @param userName
	 */
	void invalidate(String userName);
	
	/**
	 * increments the user attempts
	 * @param userName
	 */
	void loginFailed(String userName);
	
	/**
	 * checks if user is blocked
	 * 
	 * @param userName
	 * @return
	 */
	boolean isBlocked(String userName);
	
	/**
	 * for external control, if user name should be used
	 * @return
	 */
	boolean isUseUserName();
	
	/**
	 * for external control, if remote address should be used
	 * @return
	 */
	boolean isUseRemoteAddress();
	
	/**
	 * clears all attempts
	 */
	void clearAttempts();
}
