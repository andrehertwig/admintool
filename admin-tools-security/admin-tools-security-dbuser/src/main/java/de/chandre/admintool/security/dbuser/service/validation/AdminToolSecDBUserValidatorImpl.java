package de.chandre.admintool.security.dbuser.service.validation;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
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
	
	@Autowired
	private AuthenticationManager authManager;
	
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
		validate(user.getFirstName(), properties.getUsers().getFirstName(), "firstName", errors);
		validate(user.getLastName(), properties.getUsers().getLastName(), "lastName", errors);
		validate(user.getPhone(), properties.getUsers().getPhone(), "phone", errors);
		validate(user.getEmail(), properties.getUsers().getEmail(), "email", errors);

		// call interceptors
		intercept(this.interceptors, user, errors);

		return errors;
	}
	
	@Override
	public Set<ATError> validatePasswordChange(String username, String currentPassword, String newPassword, String confirmPassword) {
		
		LOGGER.trace("start password validation for user: " + username != null ? username : "null-object");
		
		Set<ATError> errors = new HashSet<>();
		UsernamePasswordAuthenticationToken authReq = new UsernamePasswordAuthenticationToken(username, currentPassword);
		Authentication auth = authManager.authenticate(authReq);
		if (!auth.isAuthenticated()) {
			errors.add(new ATError("currentPassword.wrong", getMessageWithSuffix("currentPassword.wrong", null, "Current password is wrong"), "currentPassword"));
			return errors;
		}
		
		if (null != newPassword && !newPassword.equals(confirmPassword)) {
			errors.add(new ATError("confirmPassword.wrong", getMessageWithSuffix("confirmPassword.wrong", null, "Password confirmation is not equals new password."), "confirmPassword"));
		}
		
		validate(newPassword, properties.getUsers().getPassword(), "newPassword", errors);
		
		return errors;
	}
	
	@Override
	public Set<ATError> validatePasswordReset(String username, String newPassword, String confirmPassword) {
		LOGGER.trace("start password validation for user: " + username != null ? username : "null-object");
		Set<ATError> errors = new HashSet<>();
		if (null != newPassword && !newPassword.equals(confirmPassword)) {
			errors.add(new ATError("confirmPassword.wrong", getMessageWithSuffix("confirmPassword.wrong", null, "Password confirmation is not equals new password."), "confirmPassword"));
		}
		
		validate(newPassword, properties.getUsers().getPassword(), "newPassword", errors);
		return errors;
	}
}
