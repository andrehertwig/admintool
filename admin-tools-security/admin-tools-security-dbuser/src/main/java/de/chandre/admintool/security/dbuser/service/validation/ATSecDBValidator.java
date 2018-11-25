package de.chandre.admintool.security.dbuser.service.validation;

import java.util.Set;

import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.dbuser.AdminToolSecDBProperties.Validations;

/**
 * interface for validator, implemented by {@link AbstractValidator}
 * 
 * @author André
 * @since 1.2.0
 * 
 */
public interface ATSecDBValidator {
	
	void validate(String value, Validations validations, String fieldName, Set<ATError> errors);
	
	String getMessageWithSuffix(String suffix, Object[] args, String defaultMessage);
	
	String getMessage(String code, Object[] args, String defaultMessage);
}
