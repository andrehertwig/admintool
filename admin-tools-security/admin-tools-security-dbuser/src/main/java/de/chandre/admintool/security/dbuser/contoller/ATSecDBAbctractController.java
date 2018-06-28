package de.chandre.admintool.security.dbuser.contoller;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;

import de.chandre.admintool.core.controller.AbstractAdminController;
import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.dbuser.service.validation.ATSecDBValidator;

/**
 * 
 * @author André
 * @since 1.1.7
 *
 */
public class ATSecDBAbctractController extends AbstractAdminController {
	
	@Autowired(required=false)
    private MessageSource messageSource;
	
	/**
	 * logging the error and creates error output
	 * 
	 * @param e
	 * @param logger
	 * @param validator
	 * @param key
	 * @param suffix
	 * @param defaultMessagePrefix
	 * @return
	 */
	protected <E extends Throwable, V extends ATSecDBValidator> Set<ATError> handleException(E e, Log logger, V validator, 
			String key, String suffix, String defaultMessagePrefix) {
		logger.error(e.getMessage(), e);
		Set<ATError> errors = new HashSet<>(1);
		errors.add(new ATError(key, 
				validator.getMessageWithSuffix(suffix, null, defaultMessagePrefix + ": "+ e.getMessage())));
		return errors;
	}
	
	protected <E extends Throwable> Set<ATError> handleException(E e, Log logger, 
			String key, String messageKey, String defaultMessagePrefix) {
		logger.error(e.getMessage(), e);
		Set<ATError> errors = new HashSet<>(1);
		if (null != messageSource) {
			errors.add(new ATError(key, 
					messageSource.getMessage(key, null, defaultMessagePrefix + ": "+ e.getMessage(), LocaleContextHolder.getLocale())));
		}
		
		return errors;
	}
}
