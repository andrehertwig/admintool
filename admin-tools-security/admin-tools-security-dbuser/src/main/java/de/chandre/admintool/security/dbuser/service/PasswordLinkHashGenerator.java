package de.chandre.admintool.security.dbuser.service;

/**
 * 
 * @author André
 * @since 1.2.0
 *
 */
public interface PasswordLinkHashGenerator {
	
	/**
	 * should return a unique string (hash or UUID) to identify a user when password reset request has been executed
	 * @return a String 
	 */
	String generatePasswordLinkHash();
}
