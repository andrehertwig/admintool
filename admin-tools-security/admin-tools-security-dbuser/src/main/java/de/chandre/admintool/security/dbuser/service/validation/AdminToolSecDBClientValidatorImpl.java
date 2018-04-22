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
import de.chandre.admintool.security.dbuser.domain.Client;

/**
 * 
 * @author André
 * @since 1.1.7
 *
 */
@Service
public class AdminToolSecDBClientValidatorImpl extends AbstractValidator<Client> implements AdminToolSecDBClientValidator {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolSecDBClientValidatorImpl.class);

	private static final String AREA = "clients.";
	
	@Autowired
	private AdminToolSecDBProperties properties;
	
	@Autowired(required=false)
	private List<AdminToolValidationInterceptor<Client>> interceptors;
	
	@PostConstruct
	private void init() {
		super.sortInterceptors(interceptors);
	}
	
	@Override
	protected String getMessageArea() {
		return AREA;
	}
	
	@Override
	public Set<ATError> validate(Client client) {
		
		LOGGER.trace("start validation for client: " + client != null ? client.getName() : "null-object");

		Set<ATError> errors = new HashSet<>();
		validateDomainObject(client, errors);
		if (!CollectionUtils.isEmpty(errors)) {
			return errors;
		}

		validate(client.getName(), properties.getClients().getName(), "name", errors);
		validate(client.getDisplayName(), properties.getClients().getDisplayName(), "displayName", errors);
		validate(client.getDescription(), properties.getClients().getDescription(), "description", errors);

		// call interceptors
		intercept(this.interceptors, client, errors);
		return errors;
	}

}
