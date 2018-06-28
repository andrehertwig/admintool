package de.chandre.admintool.security.dbuser.service.validation;

import java.util.Set;

import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.dbuser.domain.User;

/**
 * 
 * @author André
 * @since 1.1.7
 */
public interface AdminToolSecDBUserValidator extends ATSecDBValidator {

	/**
	 * user validation against configured rules
	 * 
	 * @param user
	 * @param validatePassword
	 * @return
	 */
	Set<ATError> validate(User user, boolean validatePassword);

	/**
	 * validates the user and current password against AuthenticationManager and the new passwords against its validation rules
	 * 
	 * @param userName
	 * @param currentPassword
	 * @param newPassword
	 * @param confirmPassword
	 * @return
	 */
	Set<ATError> validatePasswordChange(String userName, String currentPassword, String newPassword,
			String confirmPassword);

	/**
	 * just validates the passwords against its validation rules
	 *  
	 * @param username
	 * @param newPassword
	 * @param confirmPassword
	 * @return
	 */
	Set<ATError> validatePasswordReset(String username, String newPassword, String confirmPassword);
}
