package de.chandre.admintool.security.dbuser.contoller;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
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
 * @since 1.2.0
 *
 */
public class ATSecDBAbctractController extends AbstractAdminController {
	
	@Autowired(required=false)
    private MessageSource messageSource;
	
	/**
	 * logging the error and creates {@link ATError} list output
	 * @param e
	 * @param logger
	 * @param key
	 * @param messageKey
	 * @param defaultMessagePrefix
	 * @return
	 */
	protected <E extends Throwable> Set<ATError> handleException(E e, Log logger, 
			String key, String messageKey, String defaultMessagePrefix) {
		return handleException(e, logger, null, key, null, defaultMessagePrefix, new Object[] {});
	}
	
	/**
	 * logging the error and creates {@link ATError} list output
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
		return handleException(e, logger, validator, key, suffix, defaultMessagePrefix, new Object[] {});
	}
	
	/**
	 * logging the error and creates {@link ATError} list output 
	 * 
	 * @param e - required: the exception
	 * @param logger - optional: (could be null), if not null, exception will be logged as error
	 * @param validator - optional: (could be null)
	 * @param key - required: the error key
	 * @param suffix the suffix for resource message key (prefix and area is managed in validator), will be ignored if validator is null
	 * @param defaultMessagePrefix message as prefix combined with exception.getMessage if no message resource has been found
	 * @param arguments additional arguments
	 * @return list of errors
	 * 
	 * @see #handleException(Throwable, Log, String, String, String)
	 * @see #handleException(Throwable, Log, ATSecDBValidator, String, String, String)
	 */
	protected <E extends Throwable, V extends ATSecDBValidator> Set<ATError> handleException(E e, Log logger, V validator, 
			String key, String suffix, String defaultMessagePrefix, Object... arguments) {
		if(null != logger) {
			logger.error(e.getMessage(), e);
		}
		Set<ATError> errors = new HashSet<>(1);
		if (null != validator) {
			errors.add(new ATError(key, 
					validator.getMessageWithSuffix(suffix, arguments, defaultMessagePrefix + ": "+ e.getMessage())));
		}
		else if (null != messageSource) {
			errors.add(new ATError(key, 
					messageSource.getMessage(key, null, defaultMessagePrefix + ": "+ e.getMessage(), LocaleContextHolder.getLocale())));
		} else {
			errors.add(new ATError(key, defaultMessagePrefix));
		}
		return errors;
	}
	
	/**
	 * creates {@link ATError} list output
	 * @param validator
	 * @param key
	 * @param suffix
	 * @param defaultMessagePrefix
	 * @param arguments
	 * @return
	 */
	protected <V extends ATSecDBValidator> Set<ATError> createError(V validator, String key, String suffix, String defaultMessagePrefix, Object... arguments ) {
		Set<ATError> errors = new HashSet<>(1);
		if (null != validator) {
			errors.add(new ATError(key, 
					validator.getMessageWithSuffix(suffix, arguments, defaultMessagePrefix + StringUtils.join(arguments, ','))));
		} else if (null != messageSource) {
			errors.add(new ATError(key, 
					messageSource.getMessage(key, null, defaultMessagePrefix, LocaleContextHolder.getLocale())));
		} else {
			errors.add(new ATError(key, defaultMessagePrefix));
		}
		return errors;
	}
}
