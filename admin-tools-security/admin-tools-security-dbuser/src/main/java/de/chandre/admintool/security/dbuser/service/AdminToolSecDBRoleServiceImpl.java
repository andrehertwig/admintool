package de.chandre.admintool.security.dbuser.service;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import de.chandre.admintool.core.sec.ATInitRole;
import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.dbuser.Constants;
import de.chandre.admintool.security.dbuser.auth.AccessRelationTO;
import de.chandre.admintool.security.dbuser.domain.ATRole;
import de.chandre.admintool.security.dbuser.repo.RoleRepository;
import de.chandre.admintool.security.dbuser.repo.UserGroupRepository;
import de.chandre.admintool.security.dbuser.service.validation.AdminToolSecDBRoleValidator;

/**
 * 
 * @author André
 * @since 1.2.0
 */
@Service("adminToolSecDBRoleService")
@Transactional
public class AdminToolSecDBRoleServiceImpl implements AdminToolSecDBRoleService {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolSecDBRoleServiceImpl.class);

	@Autowired
	private UserGroupRepository userGroupRepository;
	
	@Autowired
	private RoleRepository roleRepository;
	
	@Autowired(required=false)
	private AdminToolSecDBRoleValidator validator;
	
	@Override
	public List<ATRole> getAllRoles() {
		return roleRepository.findAll();
	}
	
	@Override
	public List<String> getAllRoleNames() {
		return roleRepository.findAllRoleNames();
	}
	
	@Override
	public ATRole getRole(String name) {
		return roleRepository.findByName(ATRole.checkForPrefix(name));
	}
	
	@Override
	public ATRole saveRole(ATRole role) {
		return roleRepository.saveAndFlush(role);
	}
	
	@Override
	public int getAssignedUserGroupCount(ATRole role) {
		return userGroupRepository.countUserGroupsByRolesIn(Arrays.asList(role));
	}
	
	@Override
	public Set<ATError> addRolesIfNotExists(Set<ATInitRole> roles) {
		
		Map<String, ATInitRole> rolesToAdd = roles.stream().collect(Collectors.toMap(ATInitRole::getNamePrefixed, Function.identity()));
		
		List<ATRole> existingRoles = roleRepository.findByNameIn(rolesToAdd.keySet());
		if (!CollectionUtils.isEmpty(existingRoles)) {
			existingRoles.forEach(role -> {
				if(rolesToAdd.containsKey(role.getName())) {
					rolesToAdd.remove(role.getName());
					LOGGER.trace("remove role '"+role.getName()+"' from new roles to add");
				}
			});
		}
		Set<ATError> errors = new HashSet<>();
		LOGGER.debug("there are " + rolesToAdd.size() + " roles to add");
		if (!CollectionUtils.isEmpty(rolesToAdd)) {
			rolesToAdd.entrySet().forEach(roleEntry -> {
				LOGGER.trace("add new role: " + roleEntry.getValue());
				errors.addAll(
						addRole(
								roleEntry.getKey(),
								StringUtils.isNotEmpty(roleEntry.getValue().getDisplayName()) ? roleEntry.getValue().getDisplayName() : roleEntry.getValue().getName(),
								StringUtils.isNotEmpty(roleEntry.getValue().getDescription()) ? roleEntry.getValue().getDescription() :
									("automatically created: " + LocalDateTime.now().format(DateTimeFormatter.ISO_DATE_TIME)), 
								roleEntry.getValue().isActive()));
			});
		}
		return errors;
	}
	
	@Override
	public Set<ATError> updateRole(AccessRelationTO accessRelationTO) {
		Set<ATError> errors = null;
		ATRole role = getRole(StringUtils.trimToNull(accessRelationTO.getName()));
		if (null == role) {
			errors = new HashSet<>();
			errors.add(new ATError(Constants.MSG_KEY_PREFIX + "role.notFound", 
					validator.getMessageWithSuffix("notFound", null, "No user foud"), "name"));
			return errors;
		}
		if (LOGGER.isDebugEnabled()) {
			LOGGER.debug(String.format("updateding role: %s (%s)", role.getName(), role.getId()));
		}
		
		role.setDisplayName(StringUtils.trimToNull(accessRelationTO.getDisplayName()));
		role.setDescription(StringUtils.trimToNull(accessRelationTO.getDescription()));
		role.setActive(accessRelationTO.isActive());
		
		errors = validator.validate(role);
		if (CollectionUtils.isEmpty(errors)) {
			try {
				saveRole(role);
			} catch (Exception e) {
				LOGGER.debug(e.getMessage(), e);
				errors.add(new ATError(Constants.MSG_KEY_PREFIX + "role.save", 
						validator.getMessageWithSuffix("save", null, "Exception during save"), "generic"));
			}
			
		}
		return errors;
	}
	
	public Set<ATError> addRole(String name, String displayName, String description, boolean active) {
		ATRole role = new ATRole();
		role.setName(name);
		role.setDisplayName(displayName);
		role.setDescription(description);
		role.setActive(active);
		return addRole(role);
	}
	
	public Set<ATError> addRole(ATRole role) {
		ATRole role2 = getRole(role.getName());
		if (null != role2) {
			
		}
		Set<ATError> errors = validator.validate(role);
		if (null == errors || errors.size() == 0) {
			roleRepository.saveAndFlush(role);
		}
		return errors;
	}
	
	@Override
	public ATRole changeState(String name) {
		ATRole role = roleRepository.findByName(name);
		if (null != role) {
			role.setActive(!role.isActive());
			role = roleRepository.saveAndFlush(role);
			return role;
		}
		return null;
	}
	
}
