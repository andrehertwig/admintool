package de.chandre.admintool.security.dbuser.service.validation;

import java.util.Set;

import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.dbuser.domain.AbstractEntity;
import de.chandre.admintool.security.dbuser.domain.Entity;

/**
 * interface custom validation interceptors
 * 
 * @author André
 * @since 1.2.0
 *
 * @param <O> The entity to validate will be an implementation of {@link AbstractEntity}
 */
public interface AdminToolValidationInterceptor<O extends Entity> {

	/**
	 * the lower the precedence the earlier the interceptor is getting called (after standard validations)
	 * @return
	 */
	int getPrecedence();
	
	/**
	 * custom implementation of validation method.<br>
	 * if validation should fail, just add a entry to set of errors
	 * 
	 * @param object the object to validate
	 * @param errors not empty! could already contain errors
	 * @param delegator the calling service. will provide some convenience methods
	 */
	void validate(O object, Set<ATError> errors, AbstractValidator<O> delegator);
}
