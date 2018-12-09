package de.chandre.admintool.security.dbuser;

/**
 * 
 * @author André
 * @since 1.2.0
 */
public interface Constants {
	
	String MSG_KEY_PREFIX = "security.db.";
	
	String CONTENT_TPL_PATH = "/security/content/";
	
	/**
	 * Constants for initiator of communication
	 * 
	 * @author André
	 * @since 1.1.7
	 *
	 */
	public enum CommunicationProcess {
		/**
		 * user has been created (by process or user)
		 */
		CREATE_USER,
		/**
		 * reset password request was created by an admin user
		 */
		RESET_PASSWORD_REQUEST_ADMIN,
		/**
		 * reset password request was created by user
		 */
		RESET_PASSWORD_REQUEST_SELF;
	};
}
