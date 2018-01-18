package de.chandre.admintool.security.commons;

/**
 * service used in templates to get some user information
 * @author Andre
 * @since 1.0.1
 */
public interface TemplateUserService {

	/**
	 * should return the user name
	 * @return
	 */
	String getUserName();

	/**
	 * returns user details object
	 * @return
	 */
	Object getUserDetails();

	/**
	 * retunrs the principal 
	 * @return
	 */
	Object getUserPrincipal();

	/**
	 * returns if user is not logged in 
	 * @return
	 */
	boolean isAnonymous();
}
