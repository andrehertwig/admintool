package de.chandre.admintool.quartz;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import de.chandre.admintool.core.AdminToolConfig;

/**
 * @author Andre
 * @since 1.0.1
 */
@Component("adminToolQuartzConfig")
public class AdminToolQuartzConfig implements AdminToolConfig
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolQuartzConfig.class);
	
	@Value("${admintool.quartz.enabled:true}")
	private boolean enabled;
	
	@Value("${admintool.quartz.hideMenuItem:false}")
	private boolean hideMenuItem;
	
	@Value("${admintool.quartz.stopSchedulerAllowed:true}")
	private boolean stopSchedulerAllowed;
	
	@Value("${admintool.quartz.executeJobAllowed:true}")
	private boolean executeJobAllowed;
	
	@Value("${admintool.quartz.interruptJobAllowed:true}")
	private boolean interruptJobAllowed;
	
	@Value("${admintool.quartz.changeJobInfoAllowed:true}")
	private boolean changeJobInfoAllowed;
	
	@Value("${admintool.quartz.changeTriggerAllowed:true}")
	private boolean changeTriggerAllowed;
	
	@Value("${admintool.quartz.changetTriggerStateAllowed:true}")
	private boolean changetTriggerStateAllowed;
	
	@Value("${admintool.quartz.interruptTriggerAllowed:true}")
	private boolean interruptTriggerAllowed;
	
	@Value("${admintool.quartz.addTriggerAllowed:true}")
	private boolean addTriggerAllowed;
	
	@Value("${admintool.quartz.removeTriggerAllowed:true}")
	private boolean removeTriggerAllowed;
	
	@Value("#{'${admintool.quartz.securityRoles.config:}'.split(';')}")
	private Set<String> securityRolesConfig = new HashSet<>();
	
	@Value("#{'${admintool.quartz.securityRoles.jobs:}'.split(';')}")
	private Set<String> securityRolesJobs = new HashSet<>();
	
	@Value("${admintool.quartz.componentPosition:}")
	private Integer componentPosition;
	
	/**
	 * @return the hideMenuItem
	 */
	public boolean isHideMenuItem() {
		return hideMenuItem;
	}

	/**
	 * @param hideMenuItem the hideMenuItem to set
	 */
	public void setHideMenuItem(boolean hideMenuItem) {
		this.hideMenuItem = hideMenuItem;
	}

	/**
	 * @return the stopSchedulerAllowed
	 */
	public boolean isStopSchedulerAllowed() {
		return stopSchedulerAllowed;
	}

	/**
	 * @param stopSchedulerAllowed the stopSchedulerAllowed to set
	 */
	public void setStopSchedulerAllowed(boolean stopSchedulerAllowed) {
		this.stopSchedulerAllowed = stopSchedulerAllowed;
	}

	/**
	 * @return the executeJobAllowed
	 */
	public boolean isExecuteJobAllowed() {
		return executeJobAllowed;
	}

	/**
	 * @param executeJobAllowed the executeJobAllowed to set
	 */
	public void setExecuteJobAllowed(boolean executeJobAllowed) {
		this.executeJobAllowed = executeJobAllowed;
	}

	/**
	 * @return the interruptJobAllowed
	 */
	public boolean isInterruptJobAllowed() {
		return interruptJobAllowed;
	}

	/**
	 * @param interruptJobAllowed the interruptJobAllowed to set
	 */
	public void setInterruptJobAllowed(boolean interruptJobAllowed) {
		this.interruptJobAllowed = interruptJobAllowed;
	}

	/**
	 * 
	 * @return the changeJobInfoAllowed
	 */
	public boolean isChangeJobInfoAllowed() {
		return changeJobInfoAllowed;
	}

	/**
	 * @param changeJobInfoAllowed the changeJobInfoAllowed to set
	 */
	public void setChangeJobInfoAllowed(boolean changeJobInfoAllowed) {
		this.changeJobInfoAllowed = changeJobInfoAllowed;
	}

	/**
	 * @return the changeTriggerAllowed
	 */
	public boolean isChangeTriggerAllowed() {
		return changeTriggerAllowed;
	}

	/**
	 * @param changeTriggerAllowed the changeTriggerAllowed to set
	 */
	public void setChangeTriggerAllowed(boolean changeTriggerAllowed) {
		this.changeTriggerAllowed = changeTriggerAllowed;
	}

	/**
	 * @return the changetTriggerStateAllowed
	 */
	public boolean isChangetTriggerStateAllowed() {
		return changetTriggerStateAllowed;
	}

	/**
	 * @param changetTriggerStateAllowed the changetTriggerStateAllowed to set
	 */
	public void setChangetTriggerStateAllowed(boolean changetTriggerStateAllowed) {
		this.changetTriggerStateAllowed = changetTriggerStateAllowed;
	}

	/**
	 * @return the interruptTriggerAllowed
	 */
	public boolean isInterruptTriggerAllowed() {
		return interruptTriggerAllowed;
	}

	/**
	 * @param interruptTriggerAllowed the interruptTriggerAllowed to set
	 */
	public void setInterruptTriggerAllowed(boolean interruptTriggerAllowed) {
		this.interruptTriggerAllowed = interruptTriggerAllowed;
	}

	/**
	 * @return the addTriggerAllowed
	 */
	public boolean isAddTriggerAllowed() {
		return addTriggerAllowed;
	}

	/**
	 * @param addTriggerAllowed the addTriggerAllowed to set
	 */
	public void setAddTriggerAllowed(boolean addTriggerAllowed) {
		this.addTriggerAllowed = addTriggerAllowed;
	}

	/**
	 * @return the removeTriggerAllowed
	 */
	public boolean isRemoveTriggerAllowed() {
		return removeTriggerAllowed;
	}

	/**
	 * @param removeTriggerAllowed the removeTriggerAllowed to set
	 */
	public void setRemoveTriggerAllowed(boolean removeTriggerAllowed) {
		this.removeTriggerAllowed = removeTriggerAllowed;
	}

	/**
	 * @param enabled the enabled to set
	 */
	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	/**
	 * @return the securityRolesConfig
	 * @since 1.0.1
	 */
	public Set<String> getSecurityRolesConfig() {
		return securityRolesConfig;
	}

	/**
	 * @return the securityRolesJobs
	 * @since 1.0.1
	 */
	public Set<String> getSecurityRolesJobs() {
		return securityRolesJobs;
	}

	@Override
	public void printConfig() {
		LOGGER.debug(this.toString());
	}

	@Override
	public boolean isEnabled() {
		return this.enabled;
	}
	
	/**
	 * @return the componentPosition
	 * @since 1.0.1
	 */
	public Integer getComponentPosition() {
		return componentPosition;
	}

	/**
	 * @param componentPosition the componentPosition to set
	 * @since 1.0.1
	 */
	public void setComponentPosition(Integer componentPosition) {
		this.componentPosition = componentPosition;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AdminToolQuartzConfig [enabled=").append(enabled).append(", hideMenuItem=").append(hideMenuItem)
				.append(", stopSchedulerAllowed=").append(stopSchedulerAllowed).append(", executeJobAllowed=")
				.append(executeJobAllowed).append(", interruptJobAllowed=").append(interruptJobAllowed)
				.append(", changeJobInfoAllowed=").append(changeJobInfoAllowed).append(", changeTriggerAllowed=")
				.append(changeTriggerAllowed).append(", changetTriggerStateAllowed=").append(changetTriggerStateAllowed)
				.append(", interruptTriggerAllowed=").append(interruptTriggerAllowed).append(", addTriggerAllowed=")
				.append(addTriggerAllowed).append(", removeTriggerAllowed=").append(removeTriggerAllowed)
				.append(", securityRolesConfig=").append(securityRolesConfig).append(", securityRolesJobs=")
				.append(securityRolesJobs).append(", componentPosition=").append(componentPosition).append("]");
		return builder.toString();
	}
	
}
