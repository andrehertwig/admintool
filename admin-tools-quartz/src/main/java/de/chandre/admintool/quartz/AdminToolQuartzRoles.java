package de.chandre.admintool.quartz;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.springframework.stereotype.Component;

import de.chandre.admintool.core.sec.ATInitRole;
import de.chandre.admintool.core.sec.AdminToolRoles;
import de.chandre.admintool.core.sec.ATInitRole.ATInitRoleBuilder;

/**
 * static roles for quartz management view
 * 
 * @author Andre
 * @since 1.2.0
 */
@Component
public class AdminToolQuartzRoles implements AdminToolRoles {
	
	public static final ATInitRole ROLE_QUARTZ = ATInitRoleBuilder.builder()
			.name("QUARTZ").displayName("Quartz Role").description("This role is for the usage of Quartz view").build();
	
	
	public static final ATInitRole ROLE_QUARTZ_CONFIG = ATInitRoleBuilder.builder()
			.name("QUARTZ_CONFIG").displayName("Quartz Config Role").description("This role is for the usage of Quartz config view").build();
	
	public static final ATInitRole ROLE_QUARTZ_STOPSCHEDULER = ATInitRoleBuilder.builder()
			.name("QUARTZ_STOPSCHEDULER").displayName("Quartz StopScheduler").description("allows to stop the scheduler").build();
	
	
	public static final ATInitRole ROLE_QUARTZ_JOBS = ATInitRoleBuilder.builder()
			.name("QUARTZ_JOBS").displayName("Quartz Jobs Role").description("This role is for the usage of Quartz jobs view").build();
	
	public static final ATInitRole ROLE_QUARTZ_CHANGEJOBINFO = ATInitRoleBuilder.builder()
			.name("QUARTZ_CHANGEJOBINFO").displayName("Quartz ChangeJobInfo").description("allows change job info functionality").build();
	public static final ATInitRole ROLE_QUARTZ_EXECUTEJOB = ATInitRoleBuilder.builder()
			.name("QUARTZ_EXECUTEJOB").displayName("Quartz ExecuteJob").description("allows execute job/trigger functionality").build();
	public static final ATInitRole ROLE_QUARTZ_INTERRUPTJOB = ATInitRoleBuilder.builder()
			.name("QUARTZ_INTERRUPTJOB").displayName("Quartz InterruptJob").description("allows interrupt job functionality").build();
	public static final ATInitRole ROLE_QUARTZ_ADDTRIGGER = ATInitRoleBuilder.builder()
			.name("QUARTZ_ADDTRIGGER").displayName("Quartz AddTrigger").description("allows add trigger functionality").build();
	public static final ATInitRole ROLE_QUARTZ_CHANGETRIGGERSTATE = ATInitRoleBuilder.builder()
			.name("QUARTZ_CHANGETRIGGERSTATE").displayName("Quartz ChangeTriggerState").description("allows change trigger state functionality").build();
	public static final ATInitRole ROLE_QUARTZ_CHANGETRIGGER = ATInitRoleBuilder.builder()
			.name("QUARTZ_CHANGETRIGGER").displayName("Quartz ChangeTrigger").description("allows change trigger functionality").build();
	public static final ATInitRole ROLE_QUARTZ_INTERRUPTTRIGGER = ATInitRoleBuilder.builder()
			.name("QUARTZ_INTERRUPTTRIGGER").displayName("Quartz InterruptTrigger").description("allows interrupt trigger functionality").build();
	public static final ATInitRole ROLE_QUARTZ_REMOVETRIGGER = ATInitRoleBuilder.builder()
			.name("QUARTZ_REMOVETRIGGER").displayName("Quartz RemoveTrigger").description("allows remove trigger functionality").build();

	@Override
	public Collection<ATInitRole> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_QUARTZ, ROLE_QUARTZ_CONFIG, ROLE_QUARTZ_JOBS,
				ROLE_QUARTZ_CHANGEJOBINFO, ROLE_QUARTZ_EXECUTEJOB, ROLE_QUARTZ_INTERRUPTJOB, ROLE_QUARTZ_ADDTRIGGER,
				ROLE_QUARTZ_CHANGETRIGGERSTATE, ROLE_QUARTZ_CHANGETRIGGER, ROLE_QUARTZ_INTERRUPTTRIGGER,
				ROLE_QUARTZ_REMOVETRIGGER, ROLE_QUARTZ_STOPSCHEDULER));
	}
	
}
