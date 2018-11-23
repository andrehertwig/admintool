package de.chandre.admintool.jminix;

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
public class AdminToolJmxRoles implements AdminToolRoles {
	
	public static final ATInitRole ROLE_JMX = ATInitRoleBuilder.builder()
			.name("JMX").displayName("JMX Role").description("This role is for the usage of JMX browser").build();
	
	@Override
	public Collection<ATInitRole> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_JMX));
	}
	
}
