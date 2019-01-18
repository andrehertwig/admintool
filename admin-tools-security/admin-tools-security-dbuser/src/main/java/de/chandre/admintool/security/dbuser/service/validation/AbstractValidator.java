package de.chandre.admintool.security.dbuser.service.validation;

import java.io.Serializable;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import javax.annotation.PostConstruct;
import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.util.CollectionUtils;

import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.commons.TemplateUserService;
import de.chandre.admintool.security.dbuser.AdminToolSecDBProperties.Validations;
import de.chandre.admintool.security.dbuser.Constants;
import de.chandre.admintool.security.dbuser.domain.ATUser;
import de.chandre.admintool.security.dbuser.domain.Entity;

/**
 * Abstract class with common validation methods and message finding
 * @author André
 * @since 1.2.0
 *
 * @param <O> type of persisted entity
 */
public abstract class AbstractValidator<O extends Entity> implements Constants, ATSecDBValidator {
	
	private static final Log LOGGER = LogFactory.getLog(AbstractValidator.class);
	
	@Autowired(required=false)
    private MessageSource messageSource;
	
	@Autowired
	private TemplateUserService templateUserService;
	
	protected Validator validator;
	
	@PostConstruct
	private void init() {
		validator = Validation.buildDefaultValidatorFactory().getValidator();
	}
	
	/**
	 * sorts the interceptors against its precedence
	 * @param interceptors
	 */
	protected <I extends AdminToolValidationInterceptor<O>> void sortInterceptors(List<I> interceptors) {
		
		if (!CollectionUtils.isEmpty(interceptors)) {
			int amount = interceptors != null ? interceptors.size() : 0;
			LOGGER.debug(amount + " interceptors configured for " + getMessageArea());
			Collections.sort(interceptors, new Comparator<I>() {
				@Override
				public int compare(I o1, I o2) {
					return Integer.compare(o1.getPrecedence(), o2.getPrecedence());
				}
			});
			for (AdminToolValidationInterceptor<O> interceptor : interceptors) {
				LOGGER.debug("    precedence: " + interceptor.getPrecedence() + ", class: " + interceptor.getClass().getSimpleName());
			}
		}
	}
	
	/**
	 * calls the validate method on interceptors if not empty
	 * 
	 * @param interceptors
	 * @param user
	 * @param errors
	 */
	protected <I extends AdminToolValidationInterceptor<O>> void intercept(List<I> interceptors, O user, Set<ATError> errors) {
		if (!CollectionUtils.isEmpty(interceptors)) {
			 for (AdminToolValidationInterceptor<O> interceptor : interceptors) {
				 LOGGER.trace("calling validation interceptor's validate for: " + interceptor.getClass());
				 interceptor.validate(user, errors, this);
			}
		 }
	}
	
	/**
	 * should return a string with a "." at the end<br>
	 * resulting message key will be <code>{@link Constants#MSG_KEY_PREFIX} + {@link #getMessageArea()} + fieldName + errorType;</code>
	 * @return
	 */
	protected abstract String getMessageArea();
	
	/**
	 * validates a domain object with javax.validation annotations
	 * 
	 * @param domainObject
	 * @param errors
	 */
	protected <S extends Serializable> void validateDomainObject(S domainObject, Set<ATError> errors) {
		Set<ConstraintViolation<S>> constraintViolations = validator.validate( domainObject );
		if (CollectionUtils.isEmpty(constraintViolations)) {
			return;
		}
		for ( ConstraintViolation<S> violation : constraintViolations ) {
			String type = violation.getConstraintDescriptor().getAnnotation().annotationType().getSimpleName();
			String attrPath = violation.getPropertyPath().toString();
			String msgKey = attrPath + "." + type;
			if (null != violation.getMessage() && violation.getMessage().startsWith(MSG_KEY_PREFIX)) {
				msgKey = violation.getMessage();
			}
			errors.add(new ATError((attrPath + "." + type), getMessage(msgKey, new Object[]{attrPath}, violation.getMessage()), attrPath));
		}
	}
	
	/**
	 * 
	 * @param value
	 * @param validations
	 * @param fieldName
	 * @return
	 */
	public void validate(String value, Validations validations, String fieldName, Set<ATError> errors) {
		if (null == errors) {
			errors = new HashSet<>();
		}
		if (validations.getMinLength() < 1 || null == validations.getPattern()) {
			//no further validations required
			return;
		}
		String errorCode = MSG_KEY_PREFIX + getMessageArea() + fieldName;
		
		if (StringUtils.isBlank(value)) {
			errors.add(new ATError(errorCode, getMessage(errorCode + ".required", null, fieldName + " is required"), fieldName));
			return;
		}
		
		if (validations.getMinLength() > 0 && value.length() < validations.getMinLength()) {
			String message = getMessage(
					errorCode + ".minLength", 
					new Object[] {validations.getMinLength()}, 
					fieldName + " is to short min-length is " + validations.getMinLength());
			errors.add(new ATError(errorCode, message, fieldName));
			LOGGER.trace(message);
		}
		if (validations.getMaxLength() > 0 && value.length() > validations.getMaxLength()) {
			String message = getMessage(
					errorCode + ".maxLength", 
					new Object[] {validations.getMaxLength()}, 
					fieldName + " is to long. max-length is " + validations.getMaxLength());
			errors.add(new ATError(errorCode, message, fieldName));
			LOGGER.trace(message);
		}
		if (null != validations.getPattern() && !validations.getPattern().matcher(value).matches()) {
			String message = getMessage(
					errorCode + ".patternMismatch", 
					new Object[] {validations.getPatternStr()},
					fieldName + " doesn't matches the required pattern");
			errors.add(new ATError(errorCode, message, fieldName));
			LOGGER.trace(message);
		}
	}
	
	public String getMessageWithSuffix(String suffix, Object[] args, String defaultMessage) {
		return getMessage(MSG_KEY_PREFIX + getMessageArea() + StringUtils.trimToEmpty(suffix), args, defaultMessage);
	}
	
	public String getMessage(String code, Object[] args, String defaultMessage) {
		if (null != messageSource) {
			ATUser user = (ATUser)templateUserService.getUserPrincipal();
			Locale locale = null != user.getLocale() ? user.getLocaleAsLocale() : LocaleContextHolder.getLocale();
			return messageSource.getMessage(code, args, defaultMessage, locale);
		}
		return defaultMessage;
	}
}
