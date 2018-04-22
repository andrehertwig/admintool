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
import de.chandre.admintool.security.dbuser.domain.UserGroup;

@Service
public class AdminToolSecDBUserGroupValidatorImpl extends AbstractValidator<UserGroup> implements AdminToolSecDBUserGroupValidator {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolSecDBUserGroupValidatorImpl.class);

	private static final String AREA = "userGroups.";
	
	@Autowired
	private AdminToolSecDBProperties properties;
	
	@Autowired(required=false)
	private List<AdminToolValidationInterceptor<UserGroup>> interceptors;
	
	@PostConstruct
	private void init() {
		super.sortInterceptors(interceptors);
	}
	
	@Override
	protected String getMessageArea() {
		return AREA;
	}
	
	@Override
	public Set<ATError> validate(UserGroup userGroup) {
		
		LOGGER.trace("start validation for userGroup: " + userGroup != null ? userGroup.getName() : "null-object");

		Set<ATError> errors = new HashSet<>();
		validateDomainObject(userGroup, errors);
		if (!CollectionUtils.isEmpty(errors)) {
			return errors;
		}
		
		validate(userGroup.getName(), properties.getUserGroups().getName(), "name", errors);
		validate(userGroup.getDisplayName(), properties.getUserGroups().getDisplayName(), "displayName", errors);
		validate(userGroup.getDescription(), properties.getUserGroups().getDescription(), "description", errors);

		// call interceptors
		intercept(this.interceptors, userGroup, errors);
		return errors;
	}
}
