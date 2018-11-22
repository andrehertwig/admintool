package de.chandre.admintool.quartz;

/**
 * interface for permission methods to delegate from service to permission handler
 * 
 * @author Andre
 * @since 1.2.0
 */
public interface ATQuartzPermissions {
	
	/**
	 * delegate method for permission handler and/or config value
	 * @return
	 */
	boolean isChangeJobInfoAllowed();
	
	/**
	 * delegate method for permission handler and/or config value
	 * @return
	 */
	boolean isExecuteJobAllowed();
	
	/**
	 * delegate method for permission handler and/or config value
	 * @return
	 */
	boolean isInterruptJobAllowed();
	
	/**
	 * delegate method for permission handler and/or config value
	 * @return
	 */
	boolean isAddTriggerAllowed();
	
	/**
	 * delegate method for permission handler and/or config value
	 * @return
	 */
	boolean isChangetTriggerStateAllowed();
	
	/**
	 * delegate method for permission handler and/or config value
	 * @return
	 */
	boolean isInterruptTriggerAllowed();

	/**
	 * delegate method for permission handler and/or config value
	 * @return
	 */
	boolean isChangeTriggerAllowed();

	/**
	 * delegate method for permission handler and/or config value
	 * @return
	 */
	boolean isRemoveTriggerAllowed();
	
	/**
	 * delegate method for permission handler and/or config value
	 * @return
	 */
	boolean isStopSchedulerAllowed();
}
