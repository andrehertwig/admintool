package de.chandre.admintool.quartz;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.quartz.CalendarIntervalTrigger;
import org.quartz.CronTrigger;
import org.quartz.DailyTimeIntervalTrigger;
import org.quartz.InterruptableJob;
import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobKey;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SchedulerMetaData;
import org.quartz.SimpleTrigger;
import org.quartz.StatefulJob;
import org.quartz.Trigger;
import org.quartz.Trigger.TriggerState;
import org.quartz.impl.matchers.GroupMatcher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.quartz.SchedulerFactoryBean;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import de.chandre.admintool.quartz.JobTriggerTA.TriggerType;

/**
 * the quart service for thymeleaf functions and actions
 * @author Andre
 *
 */
@Service("adminToolQuartzService")
public class AdminToolQuartzService
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolQuartzService.class);
	
	private static final String MISFIRE_PREFIX = "MISFIRE_INSTRUCTION_";
	
	@Autowired
	private Scheduler scheduler;
	
	@Autowired
	private SchedulerFactoryBean schedulerFactory;
	
	private String prevGroup;
	private String prevJob;
	
	/**
	 * starts the scheduler process
	 */
	public void startScheduler() {
		if(LOGGER.isDebugEnabled()) LOGGER.debug("starting scheduler factory");
		schedulerFactory.start();
	}
	/**
	 * stops the scheduler process
	 */
	public void stopScheduler() {
		if(LOGGER.isDebugEnabled()) LOGGER.debug("stopping scheduler factory");
		schedulerFactory.stop();
	}
	/**
	 * @return if scheduler is running
	 */
	public boolean isSchedulerRunning() {
		return schedulerFactory.isRunning();
	}
	/**
	 * the scheduler configuration
	 * 
	 * @return the scheduler meta data
	 * @throws SchedulerException
	 */
	public SchedulerMetaData getMetaData() throws SchedulerException {
		return scheduler.getMetaData();
	}
	/**
	 * 
	 * @return all job groups
	 * @throws SchedulerException
	 */
	public List<String> getJobGroups() throws SchedulerException {
		List<String> groups = scheduler.getJobGroupNames();
		Collections.sort(groups);
		return groups;
	}
	/**
	 * checks if the previous stored group is not equals the actual one or if previous is empty<br>
	 * requires to use the {@link #setPrevGroup(String)} method
	 * @param actualGroup the actual job group
	 * @return true if previous is empty or if previous stored group is not equals the actual group
	 */
	public boolean isPrevGoupNotEq(String actualGroup) {
		if (LOGGER.isTraceEnabled()) LOGGER.trace(String.format("check group prev: %s, act: %s", this.prevGroup, actualGroup));
		return StringUtils.isEmpty(this.prevGroup) || !actualGroup.equals(this.prevGroup);
	}
	/**
	 * sets the given group as previous group<br>
	 * dirty hack to call this method from thymeleaf to get control over row span
	 * @param prevGroup
	 * @return true
	 */
	public boolean setPrevGroup(String prevGroup) {
		if (LOGGER.isTraceEnabled()) LOGGER.trace(String.format("set previous group : %s", prevGroup));
		this.prevGroup = prevGroup;
		return true;
	}
	
	/**
	 * checks if the previous stored jobName is not equals the actual one or if previous is empty<br>
	 * requires to use the {@link #setPrevJob(String)} method
	 * @param actualJob
	 * @return true if previous is empty or if previous stored jobName is not equals the actual jobName
	 */
	public boolean isPrevJobNotEq(String actualJob) {
		if (LOGGER.isTraceEnabled()) LOGGER.trace(String.format("check job prev: %s, act: %s", this.prevJob, actualJob));
		return StringUtils.isEmpty(this.prevJob) || !actualJob.equals(this.prevJob);
	}
	/**
	 * sets the given jobName as previous jobName<br>
	 * dirty hack to call this method from thymeleaf to get control over row span
	 * @param prevJob
	 * @return true
	 */
	public boolean setPrevJob(String prevJob) {
		if (LOGGER.isTraceEnabled()) LOGGER.trace(String.format("set previous job : %s", prevJob));
		this.prevJob = prevJob;
		return true;
	}

	/**
	 * 
	 * @param group the actual job group
	 * @return all job keys for the given group
	 * @throws SchedulerException
	 */
	public Set<JobKey> getJobKeys(String group) throws SchedulerException {
		return scheduler.getJobKeys(GroupMatcher.jobGroupEquals(group));
	}
	
	public String getJobDescription(JobKey jobKey) throws SchedulerException {
		return scheduler.getJobDetail(jobKey).getDescription();
	}
	
	
	/**
	 * 
	 * @param jobKey
	 * @return all triggers for the job
	 * @throws SchedulerException
	 */
	public List<? extends Trigger> getTriggers(JobKey jobKey) throws SchedulerException {
		List<Trigger> triggers = new ArrayList<>();
		if (!CollectionUtils.isEmpty(scheduler.getTriggersOfJob(jobKey))) {
			triggers.addAll(scheduler.getTriggersOfJob(jobKey));
			Collections.sort(triggers, new Comparator<Trigger>() {
				@Override
				public int compare(Trigger o1, Trigger o2) {
					return o1.getKey().compareTo(o2.getKey());
				}
			});
		}
		return triggers;
	}
	
	/**
	 * 
	 * @param jobKey
	 * @return
	 * @throws SchedulerException
	 */
	public int getCurrentlyExecutingAmount(JobKey jobKey) throws SchedulerException {
		List<JobExecutionContext> executingJobs = scheduler.getCurrentlyExecutingJobs();
		JobDetail jobDetail = scheduler.getJobDetail(jobKey);
		int numInstances = 0;
		for (JobExecutionContext jobExecutionContext : executingJobs) {
			JobDetail execJobDetail = jobExecutionContext.getJobDetail();
			if (execJobDetail.getKey().equals(jobDetail.getKey())) {
				if (LOGGER.isTraceEnabled()) 
					LOGGER.trace(String.format("found running job for jobkey: (%s, %s)", jobKey.getGroup(), jobKey.getName()));
				++numInstances;
			}
		}
		return numInstances;
	}
	
	/**
	 * 
	 * @param jobKey
	 * @return true if all triggers of job are in paused state
	 * @throws SchedulerException
	 */
	public boolean isPaused(JobKey jobKey) throws SchedulerException {
		List<? extends Trigger> triggers = scheduler.getTriggersOfJob(jobKey);
		boolean result = true; 
		if (null != triggers && triggers.size() > 0) {
			for (Trigger trigger : triggers) {
				result = result && scheduler.getTriggerState(trigger.getKey()) == TriggerState.PAUSED;
			}
			return result;
		}
		return false;
	}
	
	/**
	 * 
	 * @param jobKey
	 * @return true if all triggers of job are in paused state
	 * @throws SchedulerException
	 */
	public boolean isOnePaused(JobKey jobKey) throws SchedulerException {
		List<? extends Trigger> triggers = scheduler.getTriggersOfJob(jobKey);
		boolean result = false; 
		if (null != triggers && triggers.size() > 0) {
			for (Trigger trigger : triggers) {
				result = result || scheduler.getTriggerState(trigger.getKey()) == TriggerState.PAUSED;
			}
			return result;
		}
		return false;
	}
	
	/**
	 * 
	 * @param trigger
	 * @return if trigger is paused
	 * @throws SchedulerException
	 */
	public boolean isPaused(Trigger trigger) throws SchedulerException {
		return scheduler.getTriggerState(trigger.getKey()) == TriggerState.PAUSED;
	}
	
	public String getTriggerStateCssClass(JobKey jobKey, Trigger trigger) throws SchedulerException {
		if (isPaused(trigger)) {
			return "btn-warning";
		}
		if (isCurrentlyRunning(jobKey, trigger)) {
			return "btn-success";
		}
		return "btn-info";
	}
	
	public String getTriggerState(JobKey jobKey, Trigger trigger) throws SchedulerException {
		if (isPaused(trigger)) {
			return "paused";
		}
		if (isCurrentlyRunning(jobKey, trigger)) {
			return "running";
		}
		return "pending";
	}
	
	public boolean isCurrentlyRunning(JobKey jobKey, Trigger trigger) throws SchedulerException {
		List<JobExecutionContext> executingJobs = scheduler.getCurrentlyExecutingJobs();
		JobDetail jobDetail = scheduler.getJobDetail(jobKey);
		Trigger triggerFound = findTrigger(jobKey.getGroup(), jobKey.getName(), trigger.getKey().getName());
		for (JobExecutionContext jobExecutionContext : executingJobs) {
			JobDetail execJobDetail = jobExecutionContext.getJobDetail();
			if (execJobDetail.getKey().equals(jobDetail.getKey())
					&& jobExecutionContext.getTrigger().getKey().equals(triggerFound.getKey())) {
				if (LOGGER.isTraceEnabled()) 
					LOGGER.trace(String.format("found running trigger for jobkey: (%s, %s, %s)", 
							jobKey.getGroup(), jobKey.getName(), trigger.getKey().getName()));
				return true;
			}
		}
		return false;
	}
	
	/**
	 * 
	 * @param trigger the actual trigger
	 * @param defaultValue a default value to be returned, if 
	 * @return return the cron expression if it's a cron trigger otherwise the defaultValue will be returned 
	 */
	public String getCronExpression(Trigger trigger, String defaultValue) {
		return isCronTrigger(trigger) ? ((CronTrigger)trigger).getCronExpression() : defaultValue;
	}
	
	/**
	 * 
	 * @param trigger
	 * @return true if it's a cron trigger
	 */
	public boolean isCronTrigger(Trigger trigger) {
		if (trigger instanceof CronTrigger) {
			return true;
		}
		return false;
	}
	
	/**
	 * 
	 * @param jobKey
	 * @return true if job class implements the InterruptableJob interface
	 * @throws SchedulerException
	 */
	public boolean isInteruptable(JobKey jobKey) throws SchedulerException {
		return InterruptableJob.class.isAssignableFrom(scheduler.getJobDetail(jobKey).getJobClass());
	}
	
	public boolean isStateful(JobKey jobKey) throws SchedulerException {
		return StatefulJob.class.isAssignableFrom(scheduler.getJobDetail(jobKey).getJobClass());
	}
	
	protected void interruptJob(String groupName, String jobName) throws SchedulerException {
		JobKey jobKey = new JobKey(jobName, groupName);
		if (isInteruptable(jobKey)) {
			List<JobExecutionContext> executingJobs = scheduler.getCurrentlyExecutingJobs();
			JobDetail jobDetail = scheduler.getJobDetail(jobKey);
			for (JobExecutionContext jobExecutionContext : executingJobs) {
				JobDetail execJobDetail = jobExecutionContext.getJobDetail();
				if (execJobDetail.getKey().equals(jobDetail.getKey())) {
					if (LOGGER.isDebugEnabled())
						LOGGER.debug(String.format("interrupting job (%s, %s)", jobDetail.getKey().getGroup(),
								jobDetail.getKey().getName()));
					((InterruptableJob) jobExecutionContext.getJobInstance()).interrupt();
				}
			}
		}
	}
	
	protected void interruptTrigger(String groupName, String jobName, String triggerName) throws SchedulerException {
		JobKey jobKey = new JobKey(jobName, groupName);
		if (isInteruptable(jobKey)) {
			List<JobExecutionContext> executingJobs = scheduler.getCurrentlyExecutingJobs();
			JobDetail jobDetail = scheduler.getJobDetail(jobKey);
			Trigger triggerFound = findTrigger(groupName, jobName, triggerName);
			for (JobExecutionContext jobExecutionContext : executingJobs) {
				if (jobExecutionContext.getJobDetail().getKey().equals(jobDetail.getKey()) 
						&& jobExecutionContext.getTrigger().getKey().equals(triggerFound.getKey())) {
					if (LOGGER.isDebugEnabled())
						LOGGER.debug(String.format("interrupting jobTrigger (%s, %s, %s)", jobDetail.getKey().getGroup(),
								jobDetail.getKey().getName(), triggerFound.getKey().getName()));
					((InterruptableJob) jobExecutionContext.getJobInstance()).interrupt();
				}
			}
		}
	}
	
	protected void changeTriggerState(String groupName, String jobName, String triggerName) throws SchedulerException {
		Trigger triggerFound = findTrigger(groupName, jobName, triggerName);
		if (null != triggerFound) {
			// pause only the one trigger of job
			changeTriggerState(triggerFound);
			if (LOGGER.isDebugEnabled())
				LOGGER.debug(String.format("pausing trigger for group: %s, job: %s, trigger: %s ", groupName, jobName,
						triggerName));
		} else {
			if (LOGGER.isDebugEnabled())
				LOGGER.debug(String.format("pausing all triggers for group: %s, job: %s", groupName, jobName));
			List<? extends Trigger> triggers = scheduler.getTriggersOfJob(new JobKey(jobName, groupName));
			for (Trigger trigger : triggers) {
				// pause all triggers of job
				changeTriggerState(trigger);
			}
		}
	}
	
	private Trigger findTrigger(String groupName, String jobName, String triggerName) throws SchedulerException {
		List<? extends Trigger> triggers = scheduler.getTriggersOfJob(new JobKey(jobName, groupName));
		Trigger triggerFound = null;
		// because don't know when the trigger comes, we have to find it first
		if (!StringUtils.isEmpty(triggerName)) {
			for (Trigger trigger : triggers) {
				if (!StringUtils.isEmpty(trigger.getKey().getName()) && trigger.getKey().getName().equals(triggerName)) {
					triggerFound = trigger;
					break;
				}
			}
		}
		return triggerFound;
	}
	
	private void changeTriggerState(Trigger trigger) throws SchedulerException {
		if (scheduler.getTriggerState(trigger.getKey()) == TriggerState.PAUSED) {
			scheduler.resumeTrigger(trigger.getKey());
		} else {
			scheduler.pauseTrigger(trigger.getKey());
		}
	}
	
	protected void triggerJob(String groupName, String jobName) throws SchedulerException {
		scheduler.triggerJob(new JobKey(jobName, groupName));
	}
	
	private JobDetail findJob(String groupName, String jobName) throws SchedulerException {
		return scheduler.getJobDetail(new JobKey(jobName, groupName));
	}
	
	
	protected boolean removeTrigger(String groupName, String jobName, String triggerName) throws SchedulerException {
		Trigger triggerFound = findTrigger(groupName, jobName, triggerName);
		if (null != triggerFound) {
			return scheduler.unscheduleJob(triggerFound.getKey());
		}
		return false;
	}
	
	
	protected JobTriggerTA getTriggerInfo(String groupName, String jobName, String triggerName) throws SchedulerException {
		JobDetail detail = findJob(groupName, jobName);
		
		JobTriggerTA jobTrigger = new JobTriggerTA();
		jobTrigger.setJobGroup(groupName);
		jobTrigger.setJobName(jobName);
		jobTrigger.setDescription(detail.getDescription());
		
		if (null == triggerName) {
			return jobTrigger;
		}
		Trigger trigger = findTrigger(groupName, jobName, triggerName);
		jobTrigger.setTriggerName(triggerName);
		jobTrigger.setTriggerGroup(trigger.getKey().getGroup());
		jobTrigger.setTriggerDescription(trigger.getDescription());
		
		jobTrigger.setMisfireInstruction(trigger.getMisfireInstruction());
		jobTrigger.setPriority(trigger.getPriority());
		jobTrigger.setCalendarName(trigger.getCalendarName());
		jobTrigger.setStartTime(trigger.getStartTime());
		
		if (isCronTrigger(trigger)) {
			addSimilarMisfireInstructionSet(jobTrigger, null, TriggerType.CRON);
			jobTrigger.setCronExpression(((CronTrigger)trigger).getCronExpression());
			jobTrigger.setTimeZone(((CronTrigger)trigger).getTimeZone());
		} else if (trigger instanceof SimpleTrigger) {
			addSimpleMisfireInstructionSet(jobTrigger, null);
			jobTrigger.setRepeatCount(((SimpleTrigger)trigger).getRepeatCount());
			jobTrigger.setRepeatInterval(((SimpleTrigger)trigger).getRepeatInterval());
		} else if (trigger instanceof CalendarIntervalTrigger) {
			addSimilarMisfireInstructionSet(jobTrigger, null, TriggerType.CALENDAR);
			jobTrigger.setRepeatInterval(((CalendarIntervalTrigger)trigger).getRepeatInterval());
			jobTrigger.setRepeatIntervalUnit(((CalendarIntervalTrigger)trigger).getRepeatIntervalUnit());
		} else if (trigger instanceof DailyTimeIntervalTrigger) {
			addSimilarMisfireInstructionSet(jobTrigger, null, TriggerType.DAILY);
			jobTrigger.setRepeatInterval(((DailyTimeIntervalTrigger)trigger).getRepeatInterval());
			jobTrigger.setRepeatIntervalUnit(((DailyTimeIntervalTrigger)trigger).getRepeatIntervalUnit());
			jobTrigger.setRepeatCount(((DailyTimeIntervalTrigger)trigger).getRepeatCount());
		}
		
		addGlobalMisfireInstructionSet(jobTrigger, null);
		
		return jobTrigger;
	}
	
	protected Collection<String> getCalendarNames() throws SchedulerException {
		return scheduler.getCalendarNames();
	}

	protected Collection<JobTriggerTA> getMisfireInstructionSets() {
		List<JobTriggerTA> instructionSets = new ArrayList<>();
		
		addSimilarMisfireInstructionSet(null, instructionSets, TriggerType.CRON);
		addSimpleMisfireInstructionSet(null, instructionSets);
		addSimilarMisfireInstructionSet(null, instructionSets, TriggerType.CALENDAR);
		addSimilarMisfireInstructionSet(null, instructionSets, TriggerType.DAILY);
		
		addGlobalMisfireInstructionSet(null, instructionSets);
		
		return instructionSets;
	}
	
	private void addGlobalMisfireInstructionSet(JobTriggerTA jobTrigger, List<JobTriggerTA> instructionSets) {
		if (null != instructionSets) {
			for (JobTriggerTA trigger : instructionSets) {
				trigger.addMisfireInstructions("IGNORE_MISFIRE_POLICY", Trigger.MISFIRE_INSTRUCTION_IGNORE_MISFIRE_POLICY);
				trigger.addMisfireInstructions("SMART_POLICY", Trigger.MISFIRE_INSTRUCTION_SMART_POLICY);
			}
		} else {
			jobTrigger.addMisfireInstructions("IGNORE_MISFIRE_POLICY", Trigger.MISFIRE_INSTRUCTION_IGNORE_MISFIRE_POLICY);
			jobTrigger.addMisfireInstructions("SMART_POLICY", Trigger.MISFIRE_INSTRUCTION_SMART_POLICY);
		}
	}
	
	private void addSimpleMisfireInstructionSet(JobTriggerTA jobTrigger, List<JobTriggerTA> instructionSets) {
		if (null == jobTrigger) {
			jobTrigger = new JobTriggerTA();
		}
		jobTrigger.setType(TriggerType.SIMPLE);
		jobTrigger.addMisfireInstructions("FIRE_NOW", SimpleTrigger.MISFIRE_INSTRUCTION_FIRE_NOW);
		jobTrigger.addMisfireInstructions("RESCHEDULE_NEXT_WITH_EXISTING_COUNT", SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_EXISTING_COUNT);
		jobTrigger.addMisfireInstructions("RESCHEDULE_NEXT_WITH_REMAINING_COUNT", SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_REMAINING_COUNT);
		jobTrigger.addMisfireInstructions("RESCHEDULE_NOW_WITH_EXISTING_REPEAT_COUNT", SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NOW_WITH_EXISTING_REPEAT_COUNT);
		jobTrigger.addMisfireInstructions("RESCHEDULE_NOW_WITH_REMAINING_REPEAT_COUNT", SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NOW_WITH_REMAINING_REPEAT_COUNT);
		if (null != instructionSets) {
			instructionSets.add(jobTrigger);
		}
		
	}
	
	private void addSimilarMisfireInstructionSet(JobTriggerTA jobTrigger, List<JobTriggerTA> instructionSets, TriggerType type) {
		if (null == jobTrigger) {
			jobTrigger = new JobTriggerTA();
		}
		jobTrigger.setType(type);
		jobTrigger.addMisfireInstructions("DO_NOTHING", CronTrigger.MISFIRE_INSTRUCTION_DO_NOTHING);
		jobTrigger.addMisfireInstructions("FIRE_ONCE_NOW", CronTrigger.MISFIRE_INSTRUCTION_FIRE_ONCE_NOW);
		if (null != instructionSets) {
			instructionSets.add(jobTrigger);
		}
	}
}
