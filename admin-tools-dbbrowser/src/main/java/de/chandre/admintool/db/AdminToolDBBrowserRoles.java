package de.chandre.admintool.db;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.springframework.stereotype.Component;

import de.chandre.admintool.core.sec.ATInitRole;
import de.chandre.admintool.core.sec.AdminToolRoles;
import de.chandre.admintool.core.sec.ATInitRole.ATInitRoleBuilder;

/**
 * 
 * @author Andre
 * @since 1.2.0
 */
@Component
public class AdminToolDBBrowserRoles implements AdminToolRoles {
	
	public static final ATInitRole ROLE_DBBROWSER = ATInitRoleBuilder.builder()
			.name("DBBROWSER").displayName("DatabaseBrowser Role").description("This role is for the usage of database browser").build();
	
	@Override
	public Collection<ATInitRole> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_DBBROWSER));
	}
	
}
