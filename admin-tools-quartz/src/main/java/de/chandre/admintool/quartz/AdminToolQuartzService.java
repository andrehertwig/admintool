package de.chandre.admintool.quartz;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.quartz.CronTrigger;
import org.quartz.InterruptableJob;
import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobKey;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SchedulerMetaData;
import org.quartz.StatefulJob;
import org.quartz.Trigger;
import org.quartz.Trigger.TriggerState;
import org.quartz.impl.matchers.GroupMatcher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.quartz.SchedulerFactoryBean;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

/**
 * the quart service for thymeleaf functions and actions
 * @author Andre
 *
 */
@Service("adminToolQuartzService")
public class AdminToolQuartzService
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolQuartzService.class);
	
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
	
	/**
	 * 
	 * @param jobKey
	 * @return all triggers for the job
	 * @throws SchedulerException
	 */
	public List<? extends Trigger> getTriggers(JobKey jobKey) throws SchedulerException {
		return scheduler.getTriggersOfJob(jobKey);
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
	 * @return if job is in paused state
	 * @throws SchedulerException
	 */
	public boolean isPaused(JobKey jobKey) throws SchedulerException {
		List<? extends Trigger> triggers = getTriggers(jobKey);
		if (triggers!= null && triggers.size() == 1) {
		    return scheduler.getTriggerState(triggers.get(0).getKey()) == TriggerState.PAUSED;
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
	
	protected void changeTriggerState(String groupName, String jobName, String triggerName) throws SchedulerException {
		List<? extends Trigger> triggers = scheduler.getTriggersOfJob(new JobKey(jobName, groupName));
		Trigger triggerFound = null;
		// because don't know when the trigger comes, we have to find it first
		for (Trigger trigger : triggers) {
			if (!StringUtils.isEmpty(trigger.getKey().getName()) && !StringUtils.isEmpty(triggerName) 
					&& trigger.getKey().getName().equals(triggerName)) {
				triggerFound = trigger;
				break;
			}
		}
		
		if (null != triggerFound) {
			// pause only the one trigger of job
			changeTriggerState(triggerFound);
			if (LOGGER.isDebugEnabled())
				LOGGER.debug(String.format("pausing trigger for group: %s, job: %s, trigger: %s ", groupName, jobName,
						triggerName));
		} else {
			if (LOGGER.isDebugEnabled())
				LOGGER.debug(String.format("pausing all triggers for group: %s, job: %s", groupName, jobName));
			for (Trigger trigger : triggers) {
				// pause all triggers of job
				changeTriggerState(trigger);
			}
		}
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
}
