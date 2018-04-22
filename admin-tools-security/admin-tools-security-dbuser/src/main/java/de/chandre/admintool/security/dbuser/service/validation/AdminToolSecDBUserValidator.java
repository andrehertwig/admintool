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

	Set<ATError> validate(User user, boolean validatePassword);
}
