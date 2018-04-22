package de.chandre.admintool.security.commons;

import java.io.Serializable;

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
	 * retunrs the principal. normally the user object.
	 * 
	 * @return
	 */
	Object getUserPrincipal();
	
	/**
	 * 
	 * @see #getUserPrincipal()
	 * @param userClass
	 * @return the casted object
	 */
	<U extends Serializable> U getUserPrincipal(Class<U> userClass);

	/**
	 * returns if user is not logged in 
	 * @return
	 */
	boolean isAnonymous();

	
}
