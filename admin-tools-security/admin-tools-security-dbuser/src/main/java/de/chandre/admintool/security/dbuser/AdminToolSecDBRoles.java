package de.chandre.admintool.security.dbuser;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.springframework.stereotype.Component;

import de.chandre.admintool.core.sec.ATInitRole;
import de.chandre.admintool.core.sec.ATInitRole.ATInitRoleBuilder;
import de.chandre.admintool.core.sec.AdminToolRoles;

/**
 * 
 * @author Andre
 * @since 1.2.0
 */
@Component
public class AdminToolSecDBRoles implements AdminToolRoles {
	
	public static final ATInitRole ROLE_CLIENT = ATInitRoleBuilder.builder()
			.name("CLIENT").displayName("ClientManagement Role").description("This role is for the usage of AccessManagment - Client view").active(true).build();
	public static final ATInitRole ROLE_ROLES = ATInitRoleBuilder.builder()
			.name("ROLES").displayName("RoleManagement Role").description("This role is for the usage of AccessManagment - Roles view").active(true).build();
	public static final ATInitRole ROLE_USERS = ATInitRoleBuilder.builder()
			.name("USERS").displayName("UserManagement Role").description("This role is for the usage of AccessManagment - Users view").active(true).build();
	public static final ATInitRole ROLE_GROUPS = ATInitRoleBuilder.builder()
			.name("GROUPS").displayName("GroupManagement Role").description("This role is for the usage of AccessManagment - UserGroups view").active(true).build();
	public static final ATInitRole ROLE_ACCMGMT = ATInitRoleBuilder.builder()
			.name("ACCMGMT").displayName("AccessManagement Role").description("This role is for the usage of AccessManagment - general").active(true).build();
	
	@Override
	public Collection<ATInitRole> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_CLIENT, ROLE_ROLES, ROLE_USERS, ROLE_GROUPS, ROLE_ACCMGMT));
	}
	
}
