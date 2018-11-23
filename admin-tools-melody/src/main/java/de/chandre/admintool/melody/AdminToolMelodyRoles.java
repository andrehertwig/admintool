package de.chandre.admintool.melody;

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
public class AdminToolMelodyRoles implements AdminToolRoles {
	
	public static final ATInitRole ROLE_MELODY = ATInitRoleBuilder.builder()
			.name("MELODY").displayName("JavaMelody Role").description("This role is for the usage of JavaMelody view").build();
	
	@Override
	public Collection<ATInitRole> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_MELODY));
	}
	
}
