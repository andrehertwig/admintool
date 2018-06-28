/**
 * 
 */
package de.chandre.admintool.security.dbuser.service.comm;

import de.chandre.admintool.security.dbuser.Constants.CommunicationProcess;

/**
 * @author André
 * @since 1.1.7
 *
 */
public interface AdminToolSecDBCommunicator {
	
	/**
	 * should send a notice to the user that password has been reseted and he/she must visit a special site to set a new one
	 * @param username
	 * @param email
	 * @param phone
	 * @param passwordLinkHash
	 * @throws SendException
	 */
	void sendResetedPasswordNotice(CommunicationProcess process, String username, String email, String phone, String passwordLinkHash) throws SendException;
}
