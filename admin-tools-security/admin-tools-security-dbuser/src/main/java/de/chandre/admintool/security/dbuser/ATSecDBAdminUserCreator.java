package de.chandre.admintool.security.dbuser;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TimeZone;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;

import de.chandre.admintool.core.sec.ATInitRole;
import de.chandre.admintool.security.dbuser.domain.ATRole;
import de.chandre.admintool.security.dbuser.domain.ATUser;
import de.chandre.admintool.security.dbuser.domain.ATUserGroup;
import de.chandre.admintool.security.dbuser.repo.RoleRepository;
import de.chandre.admintool.security.dbuser.repo.UserGroupRepository;
import de.chandre.admintool.security.dbuser.service.AdminToolSecDBRoleService;
import de.chandre.admintool.security.dbuser.service.AdminToolSecDBUserDetailsService;

/**
 * 
 * @author André
 * @since 1.2.0
 */
public class ATSecDBAdminUserCreator {
	private static final Log LOGGER = LogFactory.getLog(ATSecDBAdminUserCreator.class);

	@Autowired
	private AdminToolSecDBUserDetailsService userDetailsService;
	
	@Autowired
	private AdminToolSecDBRoleService roleService;
	
	@Autowired
	private RoleRepository roleRepository;
	
	@Autowired
	private UserGroupRepository groupRepo;
	
	@Autowired
	private AdminToolSecDBRoles roles;
	
	public void createOrUpdateAdminUserAndGroup(boolean onlyAccessManagementRoles) {
		createOrUpdateAdminUserAndGroup(null, null, null, null, null, null, null, null, null, onlyAccessManagementRoles);
	}
	
	public void createOrUpdateAdminUserAndGroup(String username, String password, String firstName, String lastName, 
			Locale locale, TimeZone timeZone,
			String userGroupName, String userGroupDisplayName, String userGroupDescription,
			boolean onlyAccessManagementRoles) {
		
		ATUser user = null;
		
		if (StringUtils.isEmpty(username)) {
			LOGGER.info("set admin username to: admin");
			username = "admin";
		} else {
			user = userDetailsService.getUser(username);
		}
		if (null == user && StringUtils.isEmpty(password)) {
			LOGGER.info("set admin password to: admin");
			username = "admin";
		}
		if (null == user && null == locale) locale = Locale.getDefault();
		if (null == user && null == timeZone) timeZone = TimeZone.getDefault();
		if (null == user && StringUtils.isEmpty(firstName)) firstName = "Ad";
		if (null == user && StringUtils.isEmpty(lastName)) lastName = "Ministrator";
		
		if (StringUtils.isEmpty(userGroupName)) {
			if(onlyAccessManagementRoles) {
				userGroupName = "ACCESSMANAGEMENT_CRUD";
			} else {
				userGroupName = "ALLROLESGROUP_CRUD";
			}
		}
		if (StringUtils.isEmpty(userGroupDisplayName)) {
			if(onlyAccessManagementRoles) {
				userGroupDisplayName = "Access Management (CRUD)";
			} else {
				userGroupDisplayName = "All Roles Group (CRUD)";
			}
		}
		if (StringUtils.isEmpty(userGroupDescription)) {
			if(onlyAccessManagementRoles) {
				userGroupDescription = "This group is supposed to do everthing in access management views";
			} else {
				userGroupDescription = "This group is supposed to do everthing in every view";
			}
		}
		
		LOGGER.info("addRolesIfNotExists");
		roleService.addRolesIfNotExists(new HashSet<>(roles.getRoles()));
		Set<String> allAccessMgmtRoleNames = roles.getRoles().stream().map(ATInitRole::getNamePrefixed).collect(Collectors.toSet());
		
		List<ATRole> assignableRoles = null;
		if (onlyAccessManagementRoles) {
			assignableRoles = roleRepository.findByNameIn(allAccessMgmtRoleNames);
		} else {
			assignableRoles = roleRepository.findAll();
		}
		
		ATUserGroup userMgmtGroup =  createOrUpdateGroup(userGroupName, userGroupDisplayName, userGroupDescription, true, assignableRoles);
		Set<ATUserGroup> assignableGroups = new HashSet<>();
		assignableGroups.add(userMgmtGroup);
		
		createOrUpdateUser(user, username, password, firstName, lastName, locale, timeZone, assignableGroups);
	}
	
	private void createOrUpdateUser(ATUser user, String username, String password, String firstName, String lastName, 
			Locale locale, TimeZone timeZone, Set<ATUserGroup> userGroupAd) {
		if (null == user) {
			LOGGER.info("creating user: " + username);
			user = new ATUser(username, password);
			user.setAccountExpiredSince(LocalDateTime.now());
		} else {
			LOGGER.info("updating user: " + username);
		}
		user.setFirstName(firstName);
		user.setLastName(lastName);
		user.setLocale(locale);
		user.setTimeZone(timeZone);
		for (ATUserGroup atUserGroup : userGroupAd) {
			user.getUserGroups().add(atUserGroup);
		}
		user = userDetailsService.saveUser(user, true);
	}

	private ATUserGroup createOrUpdateGroup(String groupName, String displayName, String desc, boolean active, List<ATRole> roles) {
		ATUserGroup userGroup = groupRepo.findByName(groupName);
		if (null == userGroup) {
			LOGGER.info("creating group: " + groupName);
			userGroup = new ATUserGroup(groupName);
		} else {
			LOGGER.info("updating group: " + groupName);
		}
		userGroup.setDisplayName(displayName);
		userGroup.setDescription(desc);
		userGroup.setActive(active);
		userGroup.setRoles(roles);
		userGroup = groupRepo.saveAndFlush(userGroup);
		return userGroup;
	}
}
