package de.chandre.admintool.properties;

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
public class AdminToolPropertiesRoles implements AdminToolRoles {
	
	public static final ATInitRole ROLE_PROPS = ATInitRoleBuilder.builder()
			.name("PROPS").displayName("Properties Role").description("This role is for the usage of properties view").build();
	
	@Override
	public Collection<ATInitRole> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_PROPS));
	}
	
}
