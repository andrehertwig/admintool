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
import de.chandre.admintool.security.dbuser.domain.Role;

/**
 * 
 * @author André
 * @since 1.1.7
 */
@Service
public class AdminToolSecDBRoleValidatorImpl extends AbstractValidator<Role> implements AdminToolSecDBRoleValidator {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolSecDBRoleValidatorImpl.class);
	
	private static final String AREA = "roles.";
	
	@Autowired
	private AdminToolSecDBProperties properties;
	
	@Autowired(required=false)
	private List<AdminToolValidationInterceptor<Role>> interceptors;
	
	@PostConstruct
	private void init() {
		super.sortInterceptors(interceptors);
	}
	
	@Override
	protected String getMessageArea() {
		return AREA;
	}

	@Override
	public Set<ATError> validate(Role role) {
		
		LOGGER.trace("start validation for role: " + role != null ? role.getName() : "null-object");

		Set<ATError> errors = new HashSet<>();
		validateDomainObject(role, errors);
		if (!CollectionUtils.isEmpty(errors)) {
			return errors;
		}
		
		validate(role.getName(), properties.getRoles().getName(), "name", errors);
		validate(role.getDisplayName(), properties.getRoles().getDisplayName(), "displayName", errors);
		validate(role.getDescription(), properties.getRoles().getDescription(), "description", errors);

		// call interceptors
		intercept(this.interceptors, role, errors);
		return errors;
	}

}
