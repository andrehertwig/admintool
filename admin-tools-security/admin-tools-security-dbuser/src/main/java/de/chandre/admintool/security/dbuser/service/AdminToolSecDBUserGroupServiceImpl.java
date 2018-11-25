package de.chandre.admintool.security.dbuser.service;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.dbuser.Constants;
import de.chandre.admintool.security.dbuser.auth.AccessRelationTO;
import de.chandre.admintool.security.dbuser.domain.ATUserGroup;
import de.chandre.admintool.security.dbuser.repo.RoleRepository;
import de.chandre.admintool.security.dbuser.repo.UserGroupRepository;
import de.chandre.admintool.security.dbuser.repo.UserRepository;
import de.chandre.admintool.security.dbuser.service.validation.AdminToolSecDBUserGroupValidator;

/**
 * 
 * @author André
 * @since 1.1.7
 */
@Service("adminToolSecDBUserGroupService")
@Transactional
public class AdminToolSecDBUserGroupServiceImpl implements AdminToolSecDBUserGroupService {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolSecDBUserGroupServiceImpl.class);

	@Autowired
	private UserRepository userRepository;
	
	@Autowired
	private UserGroupRepository userGroupRepository;
	
	@Autowired
	private RoleRepository roleRepository;
	
	@Autowired(required=false)
	private AdminToolSecDBUserGroupValidator validator;
	
	@Override
	@PreAuthorize(value="hasAnyRole(T(de.chandre.admintool.security.dbuser.AdminToolSecDBRoles).ROLE_GROUPS.getName(), T(de.chandre.admintool.security.dbuser.AdminToolSecDBRoles).ROLE_USERS.getName())")
	public List<ATUserGroup> getAllUserGroups() {
		return userGroupRepository.findAll();
	}
	
	@Override
	public ATUserGroup getUserGroup(String name) {
		return userGroupRepository.findByName(name);
	}
	
	@Override
	public ATUserGroup saveUserGroup(ATUserGroup userGroup) {
		return userGroupRepository.saveAndFlush(userGroup);
	}
	
	@Override
	public int getAssignedUserCount(ATUserGroup userGroup) {
		return userRepository.countUsersByUserGroupsIn(Arrays.asList(userGroup));
	}
	
	private Set<ATError> setAndValidateAndSave(AccessRelationTO accessRelationTO, ATUserGroup userGroup) {
		userGroup.setName(StringUtils.trimToNull(accessRelationTO.getName()));
		userGroup.setDisplayName(StringUtils.trimToNull(accessRelationTO.getDisplayName()));
		userGroup.setDescription(StringUtils.trimToNull(accessRelationTO.getDescription()));
		userGroup.setActive(accessRelationTO.isActive());

		if (!CollectionUtils.isEmpty(accessRelationTO.getRelationNames())) {
			userGroup.setRoles(roleRepository.findByNameIn(accessRelationTO.getRelationNames()));
		} else {
			userGroup.getRoles().clear();
		}
		
		Set<ATError> errors = validator.validate(userGroup);
		if (CollectionUtils.isEmpty(errors)) {
			try {
				saveUserGroup(userGroup);
			} catch (Exception e) {
				LOGGER.debug(e.getMessage(), e);
				errors.add(new ATError(Constants.MSG_KEY_PREFIX + "userGroup.save", 
						validator.getMessageWithSuffix("save", null, "Exception during save"), "generic"));
			}
		}
		return errors;
	}
	
	@Override
	@PreAuthorize(value="hasRole(T(de.chandre.admintool.security.dbuser.AdminToolSecDBRoles).ROLE_GROUPS_UPDATE.getName())")
	public Set<ATError> updateUserGroup(AccessRelationTO accessRelationTO) {
		Set<ATError> errors = null;
		ATUserGroup userGroup = getUserGroup(StringUtils.trimToNull(accessRelationTO.getName()));
		if (null == userGroup) {
			errors = new HashSet<>();
			errors.add(new ATError(Constants.MSG_KEY_PREFIX + "userGroup.notFound", 
					validator.getMessageWithSuffix("notFound", null, "No user foud"), "name"));
			return errors;
		}
		if (LOGGER.isDebugEnabled()) {
			LOGGER.debug(String.format("updateding client: %s (%s)", userGroup.getName(), userGroup.getId()));
		}
		return setAndValidateAndSave(accessRelationTO, userGroup);
	}
	
	@Override
	@PreAuthorize(value="hasRole(T(de.chandre.admintool.security.dbuser.AdminToolSecDBRoles).ROLE_GROUPS_ADD.getName())")
	public Set<ATError> addUserGroup(AccessRelationTO accessRelationTO) {
		Set<ATError> errors = null;
		if (null != getUserGroup(StringUtils.trimToNull(accessRelationTO.getName()))) {
			errors = new HashSet<>();
			errors.add(new ATError(Constants.MSG_KEY_PREFIX + "userGroup.alreadyExists", 
					validator.getMessageWithSuffix("alreadyExists", null, "The client exists already"), "name"));
			return errors;
		}
		ATUserGroup userGroup = new ATUserGroup(StringUtils.trimToNull(accessRelationTO.getName()));
		if (LOGGER.isDebugEnabled()) {
			LOGGER.debug(String.format("creating client: %s", userGroup.getName()));
		}
		return setAndValidateAndSave(accessRelationTO, userGroup);
	}
	
	@Override
	@PreAuthorize(value="hasRole(T(de.chandre.admintool.security.dbuser.AdminToolSecDBRoles).ROLE_GROUPS_UPDATE.getName())")
	public ATUserGroup changeState(String name) {
		ATUserGroup usergroup = userGroupRepository.findByName(name);
		if (null != usergroup) {
			usergroup.setActive(!usergroup.isActive());
			usergroup = userGroupRepository.saveAndFlush(usergroup);
			return usergroup;
		}
		return null;
	}
	
	@Override
	@PreAuthorize(value="hasRole(T(de.chandre.admintool.security.dbuser.AdminToolSecDBRoles).ROLE_GROUPS_REMOVE.getName())")
	public void removeByName(String name) {
		this.userGroupRepository.deleteByName(name);
	}
}
