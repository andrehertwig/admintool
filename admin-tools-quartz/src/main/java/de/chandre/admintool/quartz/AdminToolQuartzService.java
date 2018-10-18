package de.chandre.admintool.quartz;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.quartz.InterruptableJob;
import org.quartz.JobKey;
import org.quartz.SchedulerException;
import org.quartz.SchedulerMetaData;
import org.quartz.StatefulJob;
import org.quartz.Trigger;

import de.chandre.admintool.quartz.JobTriggerTO.TriggerType;

/**
 * Interface for QuartzService
 * @author Andre
 *
 */
public interface AdminToolQuartzService {

	/**
	 * starts the scheduler process
	 */
	void startScheduler();

	/**
	 * stops the scheduler process
	 */
	void stopScheduler();

	/**
	 * @return if scheduler is running
	 */
	boolean isSchedulerRunning();

	/**
	 * the scheduler configuration
	 * 
	 * @return the scheduler meta data
	 * @throws SchedulerException
	 */
	SchedulerMetaData getMetaData() throws SchedulerException;

	/**
	 * 
	 * @return all job groups
	 * @throws SchedulerException
	 */
	List<String> getJobGroups() throws SchedulerException;

	/**
	 * checks if the previous stored group is not equals the actual one or if previous is empty<br>
	 * requires to use the {@link #setPrevGroup(String)} method
	 * @param actualGroup the actual job group
	 * @return true if previous is empty or if previous stored group is not equals the actual group
	 */
	boolean isPrevGoupNotEq(String actualGroup);

	/**
	 * sets the given group as previous group<br>
	 * dirty hack to call this method from thymeleaf to get control over row span
	 * @param prevGroup
	 * @return true
	 */
	boolean setPrevGroup(String prevGroup);

	/**
	 * checks if the previous stored jobName is not equals the actual one or if previous is empty<br>
	 * requires to use the {@link #setPrevJob(String)} method
	 * @param actualJob
	 * @return true if previous is empty or if previous stored jobName is not equals the actual jobName
	 */
	boolean isPrevJobNotEq(String actualJob);

	/**
	 * sets the given jobName as previous jobName<br>
	 * dirty hack to call this method from thymeleaf to get control over row span
	 * @param prevJob
	 * @return true
	 */
	boolean setPrevJob(String prevJob);

	/**
	 * 
	 * @param group the actual job group
	 * @return all job keys for the given group
	 * @throws SchedulerException
	 */
	Set<JobKey> getJobKeys(String group) throws SchedulerException;

	/**
	 * 
	 * @param jobKey the actual job key
	 * @return the job description
	 * @throws SchedulerException
	 */
	String getJobDescription(JobKey jobKey) throws SchedulerException;

	/**
	 * 
	 * @param jobKey the actual job key
	 * @return all triggers for the job
	 * @throws SchedulerException
	 */
	List<? extends Trigger> getTriggers(JobKey jobKey) throws SchedulerException;

	/**
	 * 
	 * @param jobKey the actual job key
	 * @return amount of triggers which are currently executing
	 * @throws SchedulerException
	 */
	int getCurrentlyExecutingAmount(JobKey jobKey) throws SchedulerException;

	/**
	 * 
	 * @param jobKey the actual job key
	 * @return true if all triggers of job are in paused state
	 * @throws SchedulerException
	 */
	boolean isPaused(JobKey jobKey) throws SchedulerException;

	/**
	 * 
	 * @param jobKey the actual job key
	 * @return true if all triggers of job are in paused state
	 * @throws SchedulerException
	 */
	boolean isOnePaused(JobKey jobKey) throws SchedulerException;

	/**
	 * 
	 * @param trigger the trigger
	 * @return if trigger is paused
	 * @throws SchedulerException
	 */
	boolean isPaused(Trigger trigger) throws SchedulerException;

	/**
	 * retuns "btn-warning" for paused trigger, "btn-success" for running trigger else "btn-info" 
	 * 
	 * @param jobKey the job key
	 * @param trigger the trigger
	 * @return 
	 * @throws SchedulerException
	 */
	String getTriggerStateCssClass(JobKey jobKey, Trigger trigger) throws SchedulerException;

	/**
	 * retuns "paused", "running" or "pending" depending on the triggers state
	 * @param jobKey the job key
	 * @param trigger the trigger
	 * @return
	 * @throws SchedulerException
	 */
	String getTriggerState(JobKey jobKey, Trigger trigger) throws SchedulerException;

	/**
	 * returns if the trigger is running currently 
	 * 
	 * @param jobKey the job key
	 * @param trigger the trigger
	 * @return
	 * @throws SchedulerException
	 */
	boolean isCurrentlyRunning(JobKey jobKey, Trigger trigger) throws SchedulerException;

	/**
	 * 
	 * @param trigger the actual trigger
	 * @param defaultValue a default value to be returned, if 
	 * @return return the cron expression if it's a cron trigger otherwise the defaultValue will be returned 
	 */
	String getCronExpression(Trigger trigger, String defaultValue);

	/**
	 * 
	 * @param trigger
	 * @return true if it's a cron trigger
	 */
	boolean isCronTrigger(Trigger trigger);

	/**
	 * 
	 * @param jobKey
	 * @return true if job class implements the {@link InterruptableJob} interface
	 * @throws SchedulerException
	 */
	boolean isInteruptable(JobKey jobKey) throws SchedulerException;

	/**
	 * 
	 * @param jobKey
	 * @return true if job class implements the {@link StatefulJob} interface
	 * @throws SchedulerException
	 */
	boolean isStateful(JobKey jobKey) throws SchedulerException;

	/**
	 * interrupts all triggers of job
	 * 
	 * @param jobGroup the jobGroup
	 * @param jobName the jobName
	 * @throws SchedulerException
	 */
	void interruptJob(String jobGroup, String jobName) throws SchedulerException;

	/**
	 * interrupts the trigger i found
	 * 
	 * @param jobGroup
	 * @param jobName
	 * @param triggerGroup
	 * @param triggerName
	 * @throws SchedulerException
	 */
	void interruptTrigger(String jobGroup, String jobName, String triggerGroup, String triggerName)
			throws SchedulerException;

	/**
	 * pausing either the found trigger or all triggers of found job
	 * @param groupName
	 * @param jobName
	 * @param triggerGroup (optional)
	 * @param triggerName (optional)
	 * @throws SchedulerException
	 */
	void changeTriggerState(String groupName, String jobName, String triggerGroup, String triggerName)
			throws SchedulerException;

	/**
	 * executes the job immediately with a custom trigger
	 * @param groupName job group
	 * @param jobName job name
	 * @throws SchedulerException
	 */
	void executeJob(String groupName, String jobName) throws SchedulerException;
	
	/**
	 * executes the job immediately with a custom trigger but the data map from original trigger will be used
	 * @param groupName job group
	 * @param jobName job name
	 * @param triggerGroup trigger group
	 * @param triggerName trigger name
	 * @throws SchedulerException
	 */
	void executeJobTrigger(String groupName, String jobName, String triggerGroup, String triggerName)
			throws SchedulerException;

	/**
	 * removes trigger from job
	 * 
	 * @param groupName
	 * @param jobName
	 * @param triggerGroup
	 * @param triggerName
	 * @return false if trigger was not found or unscheduling was not successful
	 * @throws SchedulerException
	 */
	boolean removeTrigger(String groupName, String jobName, String triggerGroup, String triggerName)
			throws SchedulerException;

	/**
	 * returns the job and possibly the trigger info if triggerGroup and triggerName has beeen specified
	 * 
	 * @param groupName
	 * @param jobName
	 * @param triggerGroup (optional)
	 * @param triggerName (optional)
	 * @return
	 * @throws SchedulerException
	 */
	JobTriggerTO getTriggerInfo(String groupName, String jobName, String triggerGroup, String triggerName)
			throws SchedulerException;

	/**
	 * @return the calendars associated with quartz 
	 * @throws SchedulerException
	 */
	Collection<String> getCalendarNames() throws SchedulerException;

	/**
	 * @return 
	 * the instructionSets (MisfireInstructions) and repeatIntervalUnits for {@link TriggerType#CRON}, 
	 * {@link TriggerType#SIMPLE},{@link TriggerType#CALENDAR},{@link TriggerType#DAILY},
	 */
	Collection<JobTriggerTO> getInstructionSets();

	/**
	 * changes the job information and replaces the job
	 * 
	 * @param triggerTO
	 * @return
	 * @throws SchedulerException
	 */
	boolean changeJob(JobTriggerTO triggerTO) throws SchedulerException;

	/**
	 * changes (or adds) a trigger
	 * 
	 * @param triggerTO
	 * @param add true: if a new trigger should be added (will only work, if there is no other trigger with same identifier)
	 * @return
	 * @throws SchedulerException
	 */
	boolean changeTrigger(JobTriggerTO triggerTO, boolean add) throws SchedulerException;

}