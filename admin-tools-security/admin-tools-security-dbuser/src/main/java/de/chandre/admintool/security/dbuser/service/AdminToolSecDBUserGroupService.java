package de.chandre.admintool.security.dbuser.service;

import java.util.List;
import java.util.Set;

import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.dbuser.auth.AccessRelationTO;
import de.chandre.admintool.security.dbuser.domain.ATUserGroup;

/**
 * 
 * @author André
 * @since 1.1.7
 */
public interface AdminToolSecDBUserGroupService {

	List<ATUserGroup> getAllUserGroups();

	ATUserGroup changeState(String name);

	ATUserGroup getUserGroup(String name);

	ATUserGroup saveUserGroup(ATUserGroup userGroup);

	Set<ATError> updateUserGroup(AccessRelationTO accessRelationTO);

	Set<ATError> addUserGroup(AccessRelationTO accessRelationTO);

	void removeByName(String name);

	int getAssignedUserCount(ATUserGroup userGroup);

}
