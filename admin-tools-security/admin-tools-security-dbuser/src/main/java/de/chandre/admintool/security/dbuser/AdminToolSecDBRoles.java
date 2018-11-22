package de.chandre.admintool.security.dbuser;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.springframework.stereotype.Component;

import de.chandre.admintool.core.sec.AdminToolRoles;

/**
 * 
 * @author Andre
 * @since 1.2.0
 */
@Component
public class AdminToolSecDBRoles implements AdminToolRoles {
	
	public static String ROLE_CLIENT = "CLIENT";
	public static String ROLE_ROLES = "ROLES";
	public static String ROLE_USERS = "USERS";
	public static String ROLE_GROUPS = "GROUPS";
	public static String ROLE_ACCMGMT = "ACCMGMT";
	
	@Override
	public Collection<String> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_CLIENT, ROLE_ROLES, ROLE_USERS, ROLE_GROUPS, ROLE_ACCMGMT));
	}
	
}
