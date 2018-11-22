package de.chandre.admintool.quartz;

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
public class AdminToolQuartzRoles implements AdminToolRoles {
	
	public static String ROLE_QUARTZ = "QUARTZ";
	
	public static String ROLE_QUARTZ_CONFIG = "QUARTZ_CONFIG";
	public static String ROLE_QUARTZ_JOBS = "QUARTZ_JOBS";
	
	@Override
	public Collection<String> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_QUARTZ, ROLE_QUARTZ_CONFIG, ROLE_QUARTZ_JOBS));
	}
	
}
