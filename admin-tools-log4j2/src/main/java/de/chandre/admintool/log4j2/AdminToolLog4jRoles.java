package de.chandre.admintool.log4j2;

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
public class AdminToolLog4jRoles implements AdminToolRoles {
	
	public static final ATInitRole ROLE_LOG4J = ATInitRoleBuilder.builder()
			.name("LOG4J").displayName("Log4j Role").description("This role is for the usage of Log4jView").build();
	
	public static final ATInitRole ROLE_LOG4J_LOGGERS = ATInitRoleBuilder.builder()
			.name("LOG4J_LOGGERS").displayName("Log4j Loggers").description("This role is for the usage of Log4j loggers view and usage").build();
	public static final ATInitRole ROLE_LOG4J_CONSOLE = ATInitRoleBuilder.builder()
			.name("LOG4J_CONSOLE").displayName("Log4j Console").description("This role is for the usage of Log4j console view and usage").build();
	
	@Override
	public Collection<ATInitRole> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_LOG4J, ROLE_LOG4J_LOGGERS, ROLE_LOG4J_CONSOLE));
	}
	
}
