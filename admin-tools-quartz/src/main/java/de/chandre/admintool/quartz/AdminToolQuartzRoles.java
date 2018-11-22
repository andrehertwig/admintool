package de.chandre.admintool.quartz;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.springframework.stereotype.Component;

import de.chandre.admintool.core.sec.AdminToolRoles;

/**
 * static roles for quartz management view
 * 
 * @author Andre
 * @since 1.2.0
 */
@Component
public class AdminToolQuartzRoles implements AdminToolRoles {
	
	public static final String ROLE_QUARTZ = "QUARTZ";
	
	public static final String ROLE_QUARTZ_CONFIG = "QUARTZ_CONFIG";
	public static final String ROLE_QUARTZ_JOBS = "QUARTZ_JOBS";
	
	
	public static final String ROLE_QUARTZ_CHANGEJOBINFO = "QUARTZ_CHANGEJOBINFO";
	public static final String ROLE_QUARTZ_EXECUTEJOB = "QUARTZ_EXECUTEJOB";
	public static final String ROLE_QUARTZ_INTERRUPTJOB = "QUARTZ_INTERRUPTJOB";
	public static final String ROLE_QUARTZ_ADDTRIGGER = "QUARTZ_ADDTRIGGER";
	public static final String ROLE_QUARTZ_CHANGETRIGGERSTATE = "QUARTZ_CHANGETRIGGERSTATE";
	public static final String ROLE_QUARTZ_CHANGETRIGGER = "QUARTZ_CHANGETRIGGER";
	public static final String ROLE_QUARTZ_INTERRUPTTRIGGER = "QUARTZ_INTERRUPTTRIGGER";
	public static final String ROLE_QUARTZ_REMOVETRIGGER = "QUARTZ_REMOVETRIGGER";

	public static final String ROLE_QUARTZ_STOPSCHEDULER = "QUARTZ_STOPSCHEDULER";
	
	@Override
	public Collection<String> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_QUARTZ, ROLE_QUARTZ_CONFIG, ROLE_QUARTZ_JOBS,
				ROLE_QUARTZ_CHANGEJOBINFO, ROLE_QUARTZ_EXECUTEJOB, ROLE_QUARTZ_INTERRUPTJOB, ROLE_QUARTZ_ADDTRIGGER,
				ROLE_QUARTZ_CHANGETRIGGERSTATE, ROLE_QUARTZ_CHANGETRIGGER, ROLE_QUARTZ_INTERRUPTTRIGGER,
				ROLE_QUARTZ_REMOVETRIGGER));
	}
	
}
