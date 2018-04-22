package de.chandre.admintool.security.dbuser.service.validation;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.dbuser.AdminToolSecDBProperties;
import de.chandre.admintool.security.dbuser.domain.User;

/**
 * 
 * @author André
 * @since 1.1.7
 */
@Service
public class AdminToolSecDBUserValidatorImpl extends AbstractValidator<User> implements AdminToolSecDBUserValidator {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolSecDBUserValidatorImpl.class);

	private static final String AREA = "users.";
	
	@Autowired
	private AdminToolSecDBProperties properties;
	
	@Autowired(required=false)
	private List<AdminToolValidationInterceptor<User>> interceptors;
	
	@PostConstruct
	private void init() {
		super.sortInterceptors(interceptors);
	}
	
	@Override
	protected String getMessageArea() {
		return AREA;
	}
	
	@Override
	public Set<ATError> validate(User user, boolean validatePassword) {
		
		LOGGER.trace("start validation for user: " + user != null ? user.getUsername() : "null-object");

		Set<ATError> errors = new HashSet<>();
		validateDomainObject(user, errors);
		if (!CollectionUtils.isEmpty(errors)) {
			return errors;
		}

		validate(user.getUsername(), properties.getUsers().getUsername(), "username", errors);
		if (validatePassword) {
			validate(user.getPassword(), properties.getUsers().getPassword(), "password", errors);
		}
		validate(user.getFirstName(), properties.getUsers().getFirstName(), "firstBame", errors);
		validate(user.getLastName(), properties.getUsers().getLastName(), "lastName", errors);
		validate(user.getPhone(), properties.getUsers().getPhone(), "phone", errors);
		validate(user.getEmail(), properties.getUsers().getEmail(), "email", errors);

		// call interceptors
		intercept(this.interceptors, user, errors);

		return errors;
	}
}
