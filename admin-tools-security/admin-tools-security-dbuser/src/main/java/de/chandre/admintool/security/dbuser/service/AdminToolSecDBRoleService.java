package de.chandre.admintool.security.dbuser.service;

import java.util.List;
import java.util.Set;

import de.chandre.admintool.core.sec.ATInitRole;
import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.dbuser.auth.AccessRelationTO;
import de.chandre.admintool.security.dbuser.domain.ATRole;

/**
 * 
 * @author André
 * @since 1.2.0
 */
public interface AdminToolSecDBRoleService {

	List<ATRole> getAllRoles();

	ATRole changeState(String name);

	List<String> getAllRoleNames();

	ATRole getRole(String name);

	ATRole saveRole(ATRole role);

	Set<ATError> updateRole(AccessRelationTO accessRelationTO);

	Set<ATError> addRolesIfNotExists(Set<ATInitRole> roles);

	int getAssignedUserGroupCount(ATRole role);

}
