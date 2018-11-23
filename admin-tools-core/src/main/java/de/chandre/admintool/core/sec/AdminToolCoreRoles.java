package de.chandre.admintool.core.sec;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.springframework.stereotype.Component;

import de.chandre.admintool.core.sec.ATInitRole.ATInitRoleBuilder;
import de.chandre.admintool.core.sec.AdminToolRoles;

/**
 * 
 * @author Andre
 * @since 1.2.0
 */
@Component
public class AdminToolCoreRoles implements AdminToolRoles {
	
	public static final ATInitRole ROLE_ATCORE = ATInitRoleBuilder.builder()
			.name("ATCORE").displayName("AdminTool Core Role").description("This is used for everthing which has no special role").active(true).build();
	
	@Override
	public Collection<ATInitRole> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_ATCORE));
	}
	
}
