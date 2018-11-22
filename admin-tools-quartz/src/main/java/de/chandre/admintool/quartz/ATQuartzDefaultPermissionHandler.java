package de.chandre.admintool.quartz;

import org.springframework.beans.factory.annotation.Autowired;

import de.chandre.admintool.core.sec.ATAbstractPermissionHandler;

/**
 * 
 * @author André
 * @since 1.1.7
 *
 */
public class ATQuartzDefaultPermissionHandler extends ATAbstractPermissionHandler implements ATQuartzPermissionHandler {
	
	@Autowired
	private AdminToolQuartzConfig config;
	

	@Override
	public boolean isStopSchedulerAllowed() {
		// TODO Auto-generated method stub
		return config.isStopSchedulerAllowed() && userHasRole(AdminToolQuartzRoles.ROLE_QUARTZ_STOPSCHEDULER);
	}

	@Override
	public boolean isChangeJobInfoAllowed() {
		return config.isChangeJobInfoAllowed() && userHasRole(AdminToolQuartzRoles.ROLE_QUARTZ_CHANGEJOBINFO);
	}

	@Override
	public boolean isExecuteJobAllowed() {
		return config.isExecuteJobAllowed() && userHasRole(AdminToolQuartzRoles.ROLE_QUARTZ_EXECUTEJOB);
	}

	@Override
	public boolean isInterruptJobAllowed() {
		return config.isInterruptJobAllowed() && userHasRole(AdminToolQuartzRoles.ROLE_QUARTZ_INTERRUPTJOB);
	}

	@Override
	public boolean isAddTriggerAllowed() {
		return config.isAddTriggerAllowed() && userHasRole(AdminToolQuartzRoles.ROLE_QUARTZ_ADDTRIGGER);
	}

	@Override
	public boolean isChangetTriggerStateAllowed() {
		return config.isChangetTriggerStateAllowed() && userHasRole(AdminToolQuartzRoles.ROLE_QUARTZ_CHANGETRIGGERSTATE);
	}

	@Override
	public boolean isInterruptTriggerAllowed() {
		return config.isInterruptTriggerAllowed() && userHasRole(AdminToolQuartzRoles.ROLE_QUARTZ_INTERRUPTTRIGGER);
	}

	@Override
	public boolean isChangeTriggerAllowed() {
		return config.isChangeTriggerAllowed() && userHasRole(AdminToolQuartzRoles.ROLE_QUARTZ_CHANGETRIGGER);
	}

	@Override
	public boolean isRemoveTriggerAllowed() {
		return config.isRemoveTriggerAllowed() && userHasRole(AdminToolQuartzRoles.ROLE_QUARTZ_REMOVETRIGGER);
	}
}
