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
	
	public static final ATInitRole ROLE_CLIENTS = ATInitRoleBuilder.builder()
			.name("CLIENTS").displayName("ClientManagement Role").description("This role is for the usage of AccessManagment - Client view").active(true).build();
	public static final ATInitRole ROLE_ROLES = ATInitRoleBuilder.builder()
			.name("ROLES").displayName("RoleManagement Role").description("This role is for the usage of AccessManagment - Roles view").active(true).build();
	public static final ATInitRole ROLE_USERS = ATInitRoleBuilder.builder()
			.name("USERS").displayName("UserManagement Role").description("This role is for the usage of AccessManagment - Users view").active(true).build();
	public static final ATInitRole ROLE_GROUPS = ATInitRoleBuilder.builder()
			.name("GROUPS").displayName("GroupManagement Role").description("This role is for the usage of AccessManagment - UserGroups view").active(true).build();
	public static final ATInitRole ROLE_ACCMGMT = ATInitRoleBuilder.builder()
			.name("ACCMGMT").displayName("AccessManagement Role").description("This role is for the usage of AccessManagment - general").active(true).build();
	
	
	public static final ATInitRole ROLE_CLIENTS_ADD = ATInitRoleBuilder.builder()
			.name("CLIENT_ADD").displayName("Client Add Role").description("This role is for the usage of AccessManagment - add Client functionality").active(true).build();
	public static final ATInitRole ROLE_CLIENTS_REMOVE = ATInitRoleBuilder.builder()
			.name("CLIENT_REMOVE").displayName("Client Remove Role").description("This role is for the usage of AccessManagment - remove Client functionality").active(true).build();
	public static final ATInitRole ROLE_CLIENTS_UPDATE = ATInitRoleBuilder.builder()
			.name("CLIENT_UPDATE").displayName("Client Update Role").description("This role is for the usage of AccessManagment - update Client functionality").active(true).build();
	
	public static final ATInitRole ROLE_USERS_ADD = ATInitRoleBuilder.builder()
			.name("USER_ADD").displayName("User Add Role").description("This role is for the usage of AccessManagment - add User functionality").active(true).build();
	public static final ATInitRole ROLE_USERS_REMOVE = ATInitRoleBuilder.builder()
			.name("USER_REMOVE").displayName("User Remove Role").description("This role is for the usage of AccessManagment - remove User functionality").active(true).build();
	public static final ATInitRole ROLE_USERS_UPDATE = ATInitRoleBuilder.builder()
			.name("USER_UPDATE").displayName("User Update Role").description("This role is for the usage of AccessManagment - update User functionality").active(true).build();
	
	public static final ATInitRole ROLE_GROUPS_ADD = ATInitRoleBuilder.builder()
			.name("GROUP_ADD").displayName("UserGroup Add Role").description("This role is for the usage of AccessManagment - add UserGroup functionality").active(true).build();
	public static final ATInitRole ROLE_GROUPS_REMOVE = ATInitRoleBuilder.builder()
			.name("GROUP_REMOVE").displayName("UserGroup Remove Role").description("This role is for the usage of AccessManagment - remove UserGroup functionality").active(true).build();
	public static final ATInitRole ROLE_GROUPS_UPDATE = ATInitRoleBuilder.builder()
			.name("GROUP_UPDATE").displayName("UserGroup Update Role").description("This role is for the usage of AccessManagment - update UserGroup functionality").active(true).build();
	
	public static final ATInitRole ROLE_ROLES_UPDATE = ATInitRoleBuilder.builder()
			.name("ROLE_UPDATE").displayName("Role Update Role").description("This role is for the usage of AccessManagment - update Role functionality").active(true).build();
	
	@Override
	public Collection<ATInitRole> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_CLIENTS, ROLE_ROLES, ROLE_USERS, ROLE_GROUPS, ROLE_ACCMGMT, 
				ROLE_CLIENTS_ADD, ROLE_CLIENTS_REMOVE, ROLE_CLIENTS_UPDATE,
				ROLE_USERS_ADD, ROLE_USERS_REMOVE, ROLE_USERS_UPDATE,
				ROLE_GROUPS_ADD, ROLE_GROUPS_REMOVE, ROLE_GROUPS_UPDATE,
				ROLE_ROLES_UPDATE));
	}
	
}
