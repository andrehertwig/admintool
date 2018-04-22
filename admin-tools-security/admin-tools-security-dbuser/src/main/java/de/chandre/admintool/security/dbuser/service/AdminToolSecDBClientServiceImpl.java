package de.chandre.admintool.security.dbuser.service;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.dbuser.Constants;
import de.chandre.admintool.security.dbuser.auth.AccessRelationTO;
import de.chandre.admintool.security.dbuser.domain.ATClient;
import de.chandre.admintool.security.dbuser.repo.ClientRepository;
import de.chandre.admintool.security.dbuser.service.validation.AdminToolSecDBClientValidator;

/**
 * 
 * @author André
 * @since 1.1.7
 */
@Service("adminToolSecDBClientService")
@Transactional
public class AdminToolSecDBClientServiceImpl implements AdminToolSecDBClientService {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolSecDBClientServiceImpl.class);

	@Autowired
	private ClientRepository clientRepository;
	
	@Autowired(required=false)
	private AdminToolSecDBClientValidator validator;
	
	@Override
	public List<ATClient> getAllClients() {
		return clientRepository.findAll();
	}
	
	@Override
	public ATClient getClient(String name) {
		return clientRepository.findByName(name);
	}
	
	@Override
	public ATClient saveClient(ATClient client) {
		return clientRepository.saveAndFlush(client);
	}
	
	private Set<ATError> setAndValidateAndSave(AccessRelationTO accessRelationTO, ATClient client) {
		client.setName(StringUtils.trimToNull(accessRelationTO.getName()));
		client.setDisplayName(StringUtils.trimToNull(accessRelationTO.getDisplayName()));
		client.setDescription(StringUtils.trimToNull(accessRelationTO.getDescription()));
		client.setActive(accessRelationTO.isActive());
		
		Set<ATError> errors = validator.validate(client);
		if (CollectionUtils.isEmpty(errors)) {
			try {
				saveClient(client);
			} catch (Exception e) {
				LOGGER.debug(e.getMessage(), e);
				errors.add(new ATError(Constants.MSG_KEY_PREFIX + "client.save", 
						validator.getMessageWithSuffix("save", null, "Exception during save"), "generic"));
			}
		}
		return errors;
	}
	
	@Override
	public Set<ATError> updateClient(AccessRelationTO accessRelationTO) {
		Set<ATError> errors = null;
		ATClient client = getClient(StringUtils.trimToNull(accessRelationTO.getName()));
		if (null == client) {
			errors = new HashSet<>();
			errors.add(new ATError(Constants.MSG_KEY_PREFIX + "client.notFound", 
					validator.getMessageWithSuffix("notFound", null, "No user foud"), "name"));
			return errors;
		}
		if (LOGGER.isDebugEnabled()) {
			LOGGER.debug(String.format("updateding client: %s (%s)", client.getName(), client.getId()));
		}
		return setAndValidateAndSave(accessRelationTO, client);
	}
	
	@Override
	public Set<ATError> addClient(AccessRelationTO accessRelationTO) {
		Set<ATError> errors = null;
		if (null != getClient(StringUtils.trimToNull(accessRelationTO.getName()))) {
			errors = new HashSet<>();
			errors.add(new ATError(Constants.MSG_KEY_PREFIX + "client.alreadyExists", 
					validator.getMessageWithSuffix("alreadyExists", null, "The client exists already"), "name"));
			return errors;
		}
		ATClient client = new ATClient(StringUtils.trimToNull(accessRelationTO.getName()));
		if (LOGGER.isDebugEnabled()) {
			LOGGER.debug(String.format("creating client: %s", client.getName()));
		}
		return setAndValidateAndSave(accessRelationTO, client);
	}

	@Override
	public ATClient changeState(String name) {
		ATClient client = clientRepository.findByName(name);
		if (null != client) {
			client.setActive(!client.isActive());
			client = clientRepository.saveAndFlush(client);
			return client;
		}
		return null;
	}
	
	@Override
	public void removeByName(String name) {
		this.clientRepository.deleteByName(name);
	}
}
