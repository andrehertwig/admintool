package de.chandre.admintool.log4j2;

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
public class AdminToolLog4jRoles implements AdminToolRoles {
	
	public static String ROLE_LOG4J = "LOG4J";
	
	public static String ROLE_LOG4J_LOGGERS = "LOG4J_LOGGERS";
	public static String ROLE_LOG4J_CONSOLE = "LOG4J_CONSOLE";
	
	@Override
	public Collection<String> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_LOG4J, ROLE_LOG4J_LOGGERS, ROLE_LOG4J_CONSOLE));
	}
	
}
