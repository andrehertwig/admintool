package de.chandre.admintool.security.dbuser.service;

import java.util.List;
import java.util.Set;

import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.dbuser.auth.AccessRelationTO;
import de.chandre.admintool.security.dbuser.domain.ATClient;

/**
 * 
 * @author André
 * @since 1.2.0
 */
public interface AdminToolSecDBClientService {

	List<ATClient> getAllClients();
	
	ATClient getClient(String name);
	
	ATClient changeState(String name);

	Set<ATError> updateClient(AccessRelationTO accessRelationTO);

	ATClient saveClient(ATClient client);

	Set<ATError> addClient(AccessRelationTO accessRelationTO);

	void removeByName(String name);

}
