package de.chandre.admintool.quartz;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.quartz.CalendarIntervalScheduleBuilder;
import org.quartz.CalendarIntervalTrigger;
import org.quartz.CronScheduleBuilder;
import org.quartz.CronTrigger;
import org.quartz.DailyTimeIntervalScheduleBuilder;
import org.quartz.DailyTimeIntervalTrigger;
import org.quartz.DisallowConcurrentExecution;
import org.quartz.DateBuilder.IntervalUnit;
import org.quartz.InterruptableJob;
import org.quartz.JobBuilder;
import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobKey;
import org.quartz.PersistJobDataAfterExecution;
import org.quartz.ScheduleBuilder;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SchedulerMetaData;
import org.quartz.SimpleScheduleBuilder;
import org.quartz.SimpleTrigger;
import org.quartz.StatefulJob;
import org.quartz.Trigger;
import org.quartz.Trigger.TriggerState;
import org.quartz.TriggerBuilder;
import org.quartz.impl.matchers.GroupMatcher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.quartz.SchedulerFactoryBean;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import de.chandre.admintool.quartz.JobTriggerTO.TriggerType;

/**
 * the quart service for thymeleaf functions and actions
 * @author Andre
 *
 */
@Service("adminToolQuartzService")
public class AdminToolQuartzServiceImpl implements AdminToolQuartzService
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolQuartzServiceImpl.class);
	
	@Autowired
	private Scheduler scheduler;
	
	@Autowired
	private SchedulerFactoryBean schedulerFactory;
	
	@Autowired
	private AdminToolQuartzConfig config;
	
	private String prevGroup;
	private String prevJob;
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#startScheduler()
	 */
	@Override
	public void startScheduler() {
		if(LOGGER.isDebugEnabled()) LOGGER.debug("starting scheduler factory");
		schedulerFactory.start();
	}
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#stopScheduler()
	 */
	@Override
	public void stopScheduler() {
		if (!config.isStopSchedulerAllowed()) {
			LOGGER.warn("not allowed to stop the scheduler");
			return;
		}
		if(LOGGER.isDebugEnabled()) LOGGER.debug("stopping scheduler factory");
		schedulerFactory.stop();
	}
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#isSchedulerRunning()
	 */
	@Override
	public boolean isSchedulerRunning() {
		return schedulerFactory.isRunning();
	}
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#getMetaData()
	 */
	@Override
	public SchedulerMetaData getMetaData() throws SchedulerException {
		return scheduler.getMetaData();
	}
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#getJobGroups()
	 */
	@Override
	public List<String> getJobGroups() throws SchedulerException {
		List<String> groups = scheduler.getJobGroupNames();
		Collections.sort(groups);
		return groups;
	}
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#isPrevGoupNotEq(java.lang.String)
	 */
	@Override
	public boolean isPrevGoupNotEq(String actualGroup) {
		if (LOGGER.isTraceEnabled()) LOGGER.trace(String.format("check group prev: %s, act: %s", this.prevGroup, actualGroup));
		return StringUtils.isEmpty(this.prevGroup) || !actualGroup.equals(this.prevGroup);
	}
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#setPrevGroup(java.lang.String)
	 */
	@Override
	public boolean setPrevGroup(String prevGroup) {
		if (LOGGER.isTraceEnabled()) LOGGER.trace(String.format("set previous group : %s", prevGroup));
		this.prevGroup = prevGroup;
		return true;
	}
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#isPrevJobNotEq(java.lang.String)
	 */
	@Override
	public boolean isPrevJobNotEq(String actualJob) {
		if (LOGGER.isTraceEnabled()) LOGGER.trace(String.format("check job prev: %s, act: %s", this.prevJob, actualJob));
		return StringUtils.isEmpty(this.prevJob) || !actualJob.equals(this.prevJob);
	}
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#setPrevJob(java.lang.String)
	 */
	@Override
	public boolean setPrevJob(String prevJob) {
		if (LOGGER.isTraceEnabled()) LOGGER.trace(String.format("set previous job : %s", prevJob));
		this.prevJob = prevJob;
		return true;
	}

	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#getJobKeys(java.lang.String)
	 */
	@Override
	public Set<JobKey> getJobKeys(String group) throws SchedulerException {
		return scheduler.getJobKeys(GroupMatcher.jobGroupEquals(group));
	}
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#getJobDescription(org.quartz.JobKey)
	 */
	@Override
	public String getJobDescription(JobKey jobKey) throws SchedulerException {
		return scheduler.getJobDetail(jobKey).getDescription();
	}
	
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#getTriggers(org.quartz.JobKey)
	 */
	@Override
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
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#getCurrentlyExecutingAmount(org.quartz.JobKey)
	 */
	@Override
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
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#isPaused(org.quartz.JobKey)
	 */
	@Override
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
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#isOnePaused(org.quartz.JobKey)
	 */
	@Override
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
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#isPaused(org.quartz.Trigger)
	 */
	@Override
	public boolean isPaused(Trigger trigger) throws SchedulerException {
		return scheduler.getTriggerState(trigger.getKey()) == TriggerState.PAUSED;
	}
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#getTriggerStateCssClass(org.quartz.JobKey, org.quartz.Trigger)
	 */
	@Override
	public String getTriggerStateCssClass(JobKey jobKey, Trigger trigger) throws SchedulerException {
		if (isPaused(trigger)) {
			return "btn-warning";
		}
		if (isCurrentlyRunning(jobKey, trigger)) {
			return "btn-success";
		}
		return "btn-info";
	}
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#getTriggerState(org.quartz.JobKey, org.quartz.Trigger)
	 */
	@Override
	public String getTriggerState(JobKey jobKey, Trigger trigger) throws SchedulerException {
		if (isPaused(trigger)) {
			return "paused";
		}
		if (isCurrentlyRunning(jobKey, trigger)) {
			return "running";
		}
		return "pending";
	}
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#isCurrentlyRunning(org.quartz.JobKey, org.quartz.Trigger)
	 */
	@Override
	public boolean isCurrentlyRunning(JobKey jobKey, Trigger trigger) throws SchedulerException {
		List<JobExecutionContext> executingJobs = scheduler.getCurrentlyExecutingJobs();
		JobDetail jobDetail = scheduler.getJobDetail(jobKey);
		Trigger triggerFound = findTrigger(jobKey.getGroup(), jobKey.getName(), trigger.getKey().getGroup(), trigger.getKey().getName());
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
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#getCronExpression(org.quartz.Trigger, java.lang.String)
	 */
	@Override
	public String getCronExpression(Trigger trigger, String defaultValue) {
		return isCronTrigger(trigger) ? ((CronTrigger)trigger).getCronExpression() : defaultValue;
	}
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#isCronTrigger(org.quartz.Trigger)
	 */
	@Override
	public boolean isCronTrigger(Trigger trigger) {
		if (trigger instanceof CronTrigger) {
			return true;
		}
		return false;
	}
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#isInteruptable(org.quartz.JobKey)
	 */
	@Override
	public boolean isInteruptable(JobKey jobKey) throws SchedulerException {
		return InterruptableJob.class.isAssignableFrom(scheduler.getJobDetail(jobKey).getJobClass());
	}
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#isStateful(org.quartz.JobKey)
	 */
	@Override
	public boolean isStateful(JobKey jobKey) throws SchedulerException {
		Class<?> jobClazz = scheduler.getJobDetail(jobKey).getJobClass();
		DisallowConcurrentExecution annotation1 = jobClazz.getAnnotation(DisallowConcurrentExecution.class);
		PersistJobDataAfterExecution annotation2 = jobClazz.getAnnotation(PersistJobDataAfterExecution.class);
		return StatefulJob.class.isAssignableFrom(jobClazz) || null != annotation1 || null != annotation2;
	}
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#interruptJob(java.lang.String, java.lang.String)
	 */
	@Override
	public void interruptJob(String jobGroup, String jobName) throws SchedulerException {
		if (!config.isInterruptJobAllowed()) {
			LOGGER.warn("not allowed to interrupt any job");
			return;
		}
		JobKey jobKey = new JobKey(jobName, jobGroup);
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
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#interruptTrigger(java.lang.String, java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public void interruptTrigger(String jobGroup, String jobName, final String triggerGroup, final String triggerName) throws SchedulerException {
		if (!config.isInterruptTriggerAllowed()) {
			LOGGER.warn("not allowed to interrupt any trigger");
			return;
		}
		JobKey jobKey = new JobKey(jobName, jobGroup);
		if (isInteruptable(jobKey)) {
			List<JobExecutionContext> executingJobs = scheduler.getCurrentlyExecutingJobs();
			JobDetail jobDetail = scheduler.getJobDetail(jobKey);
			Trigger triggerFound = findTrigger(jobGroup, jobName, triggerGroup, triggerName);
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
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#changeTriggerState(java.lang.String, java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public void changeTriggerState(String groupName, String jobName, final String triggerGroup, final String triggerName) throws SchedulerException {
		if (!config.isChangetTriggerStateAllowed()) {
			LOGGER.warn("not allowed to change any trigger state");
			return;
		}
		Trigger triggerFound = findTrigger(groupName, jobName, triggerGroup, triggerName);
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
	
	private Trigger findTrigger(String jobGroup, String jobName, final String triggerGroup, final String triggerName) throws SchedulerException {
		List<? extends Trigger> triggers = scheduler.getTriggersOfJob(new JobKey(jobName, jobGroup));
		if (!CollectionUtils.isEmpty(triggers)) {
			Optional<? extends Trigger> tr = triggers.stream()
					.filter(trigger -> trigger.getKey().getGroup().equals(triggerGroup)
							&& trigger.getKey().getName().equals(triggerName))
					.findFirst();
			if (tr.isPresent()) {
				return tr.get();
			}
		}
		return null;
	}
	
	private void changeTriggerState(Trigger trigger) throws SchedulerException {
		if (scheduler.getTriggerState(trigger.getKey()) == TriggerState.PAUSED) {
			scheduler.resumeTrigger(trigger.getKey());
		} else {
			scheduler.pauseTrigger(trigger.getKey());
		}
	}
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#triggerJob(java.lang.String, java.lang.String)
	 */
	@Override
	public void triggerJob(String groupName, String jobName) throws SchedulerException {
		if (!config.isExecuteJobAllowed()) {
			LOGGER.warn("not allowed to execute any job");
			return;
		}
		scheduler.triggerJob(new JobKey(jobName, groupName));
	}
	
	private JobDetail findJob(String groupName, String jobName) throws SchedulerException {
		return scheduler.getJobDetail(new JobKey(jobName, groupName));
	}
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#removeTrigger(java.lang.String, java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public boolean removeTrigger(String groupName, String jobName, final String triggerGroup, final String triggerName) throws SchedulerException {
		if (!config.isRemoveTriggerAllowed()) {
			LOGGER.warn("not allowed to remove any trigger");
			return false;
		}
		Trigger triggerFound = findTrigger(groupName, jobName, triggerGroup, triggerName);
		if (null != triggerFound) {
			return scheduler.unscheduleJob(triggerFound.getKey());
		}
		return false;
	}
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#getTriggerInfo(java.lang.String, java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public JobTriggerTO getTriggerInfo(String groupName, String jobName, final String triggerGroup, final String triggerName) throws SchedulerException {
		JobDetail detail = findJob(groupName, jobName);
		
		JobTriggerTO triggerTO = new JobTriggerTO();
		triggerTO.setJobGroup(groupName);
		triggerTO.setJobName(jobName);
		triggerTO.setDescription(detail.getDescription());
		
		triggerTO.setJobData(detail.getJobDataMap());
		
		if (null == triggerName) {
			return triggerTO;
		}
		Trigger trigger = findTrigger(groupName, jobName, triggerGroup, triggerName);
		triggerTO.setTriggerName(triggerName);
		triggerTO.setTriggerGroup(trigger.getKey().getGroup());
		triggerTO.setTriggerDescription(trigger.getDescription());
		
		triggerTO.setMisfireInstruction(trigger.getMisfireInstruction());
		triggerTO.setPriority(trigger.getPriority());
		triggerTO.setCalendarName(trigger.getCalendarName());
		triggerTO.setStartTime(trigger.getStartTime());
		
		if (isCronTrigger(trigger)) {
			addSimilarMisfireInstructionSet(triggerTO, null, TriggerType.CRON);
			triggerTO.setCronExpression(((CronTrigger)trigger).getCronExpression());
			triggerTO.setTimeZone(((CronTrigger)trigger).getTimeZone());
		} else if (trigger instanceof SimpleTrigger) {
			addSimpleMisfireInstructionSet(triggerTO, null);
			triggerTO.setRepeatCount(((SimpleTrigger)trigger).getRepeatCount());
			triggerTO.setRepeatInterval(((SimpleTrigger)trigger).getRepeatInterval());
		} else if (trigger instanceof CalendarIntervalTrigger) {
			addSimilarMisfireInstructionSet(triggerTO, null, TriggerType.CALENDAR);
			triggerTO.setRepeatInterval(Long.valueOf(((CalendarIntervalTrigger)trigger).getRepeatInterval()));
			triggerTO.setRepeatIntervalUnit(((CalendarIntervalTrigger)trigger).getRepeatIntervalUnit());
		} else if (trigger instanceof DailyTimeIntervalTrigger) {
			addSimilarMisfireInstructionSet(triggerTO, null, TriggerType.DAILY);
			triggerTO.setRepeatInterval(Long.valueOf(((DailyTimeIntervalTrigger)trigger).getRepeatInterval()));
			triggerTO.setRepeatIntervalUnit(((DailyTimeIntervalTrigger)trigger).getRepeatIntervalUnit());
			triggerTO.setRepeatCount(((DailyTimeIntervalTrigger)trigger).getRepeatCount());
		}
		
		triggerTO.setJobData(trigger.getJobDataMap());
		
		addGlobalMisfireInstructionSet(triggerTO, null);
		
		return triggerTO;
	}
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#getCalendarNames()
	 */
	@Override
	public Collection<String> getCalendarNames() throws SchedulerException {
		return scheduler.getCalendarNames();
	}

	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#getInstructionSets()
	 */
	@Override
	public Collection<JobTriggerTO> getInstructionSets() {
		List<JobTriggerTO> instructionSets = new ArrayList<>();
		
		addSimilarMisfireInstructionSet(null, instructionSets, TriggerType.CRON);
		addSimpleMisfireInstructionSet(null, instructionSets);
		addSimilarMisfireInstructionSet(null, instructionSets, TriggerType.CALENDAR);
		addSimilarMisfireInstructionSet(null, instructionSets, TriggerType.DAILY);
		
		addGlobalMisfireInstructionSet(null, instructionSets);
		
		return instructionSets;
	}
	
	private void addGlobalMisfireInstructionSet(JobTriggerTO jobTrigger, List<JobTriggerTO> instructionSets) {
		if (null != instructionSets) {
			for (JobTriggerTO trigger : instructionSets) {
				trigger.addMisfireInstructions("IGNORE_MISFIRE_POLICY", Trigger.MISFIRE_INSTRUCTION_IGNORE_MISFIRE_POLICY);
				trigger.addMisfireInstructions("SMART_POLICY", Trigger.MISFIRE_INSTRUCTION_SMART_POLICY);
			}
		} else {
			jobTrigger.addMisfireInstructions("IGNORE_MISFIRE_POLICY", Trigger.MISFIRE_INSTRUCTION_IGNORE_MISFIRE_POLICY);
			jobTrigger.addMisfireInstructions("SMART_POLICY", Trigger.MISFIRE_INSTRUCTION_SMART_POLICY);
		}
	}
	
	private void addSimpleMisfireInstructionSet(JobTriggerTO jobTrigger, List<JobTriggerTO> instructionSets) {
		if (null == jobTrigger) {
			jobTrigger = new JobTriggerTO();
		}
		jobTrigger.setTriggerType(TriggerType.SIMPLE);
		jobTrigger.addMisfireInstructions("FIRE_NOW", SimpleTrigger.MISFIRE_INSTRUCTION_FIRE_NOW);
		jobTrigger.addMisfireInstructions("RESCHEDULE_NEXT_WITH_EXISTING_COUNT", SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_EXISTING_COUNT);
		jobTrigger.addMisfireInstructions("RESCHEDULE_NEXT_WITH_REMAINING_COUNT", SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_REMAINING_COUNT);
		jobTrigger.addMisfireInstructions("RESCHEDULE_NOW_WITH_EXISTING_REPEAT_COUNT", SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NOW_WITH_EXISTING_REPEAT_COUNT);
		jobTrigger.addMisfireInstructions("RESCHEDULE_NOW_WITH_REMAINING_REPEAT_COUNT", SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NOW_WITH_REMAINING_REPEAT_COUNT);
		
		addSimpleRepeatIntervalUnits(jobTrigger);
		if (null != instructionSets) {
			instructionSets.add(jobTrigger);
		}
		
	}
	
	private void addSimilarMisfireInstructionSet(JobTriggerTO jobTrigger, List<JobTriggerTO> instructionSets, TriggerType type) {
		if (null == jobTrigger) {
			jobTrigger = new JobTriggerTO();
		}
		jobTrigger.setTriggerType(type);
		jobTrigger.addMisfireInstructions("DO_NOTHING", CronTrigger.MISFIRE_INSTRUCTION_DO_NOTHING);
		jobTrigger.addMisfireInstructions("FIRE_ONCE_NOW", CronTrigger.MISFIRE_INSTRUCTION_FIRE_ONCE_NOW);
		
		if (type == TriggerType.CALENDAR || type == TriggerType.DAILY) {
			addSimpleRepeatIntervalUnits(jobTrigger);
			addExtendedRepeatIntervalUnits(jobTrigger);
		}
		if (null != instructionSets) {
			instructionSets.add(jobTrigger);
		}
	}
	
	private void addSimpleRepeatIntervalUnits(JobTriggerTO jobTrigger) {
		jobTrigger.addRepeatIntervalUnits("Milliseconds", IntervalUnit.MILLISECOND);
		jobTrigger.addRepeatIntervalUnits("Second", IntervalUnit.SECOND);
		jobTrigger.addRepeatIntervalUnits("Minute", IntervalUnit.MINUTE);
		jobTrigger.addRepeatIntervalUnits("Hour", IntervalUnit.HOUR);
	}
	
	private void addExtendedRepeatIntervalUnits(JobTriggerTO jobTrigger) {
		jobTrigger.addRepeatIntervalUnits("Day", IntervalUnit.DAY);
		jobTrigger.addRepeatIntervalUnits("Week", IntervalUnit.WEEK);
		jobTrigger.addRepeatIntervalUnits("Month", IntervalUnit.MONTH);
		jobTrigger.addRepeatIntervalUnits("Year", IntervalUnit.YEAR);
	}
	
	private String nvl(String check, String defaultVal) {
		return StringUtils.isEmpty(check) ? defaultVal : check;
	}
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#changeJob(de.chandre.admintool.quartz.JobTriggerTO)
	 */
	@Override
	public boolean changeJob(JobTriggerTO triggerTO) throws SchedulerException {
		if (!config.isChangeJobInfoAllowed()) {
			LOGGER.warn("not allowed to change any job info");
			return false;
		}
		JobDetail detail = findJob(triggerTO.getOriginalJobGroup(), triggerTO.getOriginalJobName());
		if (null == detail) {
			return false;
		}
		JobBuilder builder = detail.getJobBuilder()
				.withIdentity(nvl(triggerTO.getJobName(), triggerTO.getOriginalJobName()),
						nvl(triggerTO.getJobGroup(), triggerTO.getOriginalJobGroup()))
				.withDescription(triggerTO.getDescription());
		
		//Builder don't have an interface :-/ so code duplication with triggers
		if (null != triggerTO.getJobData() && !triggerTO.getJobData().isEmpty()) {
			for (Entry<String, Object> entry : triggerTO.getJobData().entrySet()) {
				if (String.class.isAssignableFrom(entry.getValue().getClass())) {
					builder.usingJobData(entry.getKey(), (String)entry.getValue());
				} else if (Boolean.class.isAssignableFrom(entry.getValue().getClass())) {
					builder.usingJobData(entry.getKey(), (Boolean)entry.getValue());
				} else if (Integer.class.isAssignableFrom(entry.getValue().getClass())) {
					builder.usingJobData(entry.getKey(), (Integer)entry.getValue());
				} else if (Long.class.isAssignableFrom(entry.getValue().getClass())) {
					builder.usingJobData(entry.getKey(), (Long)entry.getValue());
				} else if (Double.class.isAssignableFrom(entry.getValue().getClass())) {
					builder.usingJobData(entry.getKey(), (Double)entry.getValue());
				} else if (Float.class.isAssignableFrom(entry.getValue().getClass())) {
					builder.usingJobData(entry.getKey(), (Float)entry.getValue());
				}
				if (LOGGER.isDebugEnabled())
					LOGGER.debug("jobDataEntry: " + entry.getKey() +", val: "+ entry.getValue() + ", class: " + entry.getValue().getClass());
			}
		}
		
		scheduler.addJob(builder.build(), true, true);
		return true;
	}
	
	/* (non-Javadoc)
	 * @see de.chandre.admintool.quartz.AdminToolQuartzService#changeTrigger(de.chandre.admintool.quartz.JobTriggerTO, boolean)
	 */
	@Override
	public boolean changeTrigger(JobTriggerTO triggerTO, boolean add) throws SchedulerException {
		if (!config.isChangeTriggerAllowed()) {
			LOGGER.warn("not allowed to change any trigger");
			return false;
		}
		JobDetail detail = findJob(triggerTO.getOriginalJobGroup(), triggerTO.getOriginalJobName());
		if (null == detail) {
			return false;
		}
		Trigger trigger = findTrigger(triggerTO.getOriginalJobGroup(), triggerTO.getOriginalJobName(), 
				triggerTO.getOriginalTriggerGroup(), triggerTO.getOriginalTriggerName());
		if(null == trigger) {
			if (add) {
				this.scheduler.scheduleJob(buildTrigger(detail, null, triggerTO));
				return true;
			}
			return false;
		}
		this.scheduler.rescheduleJob(trigger.getKey(), buildTrigger(detail, trigger, triggerTO));
		return true;
	}
	
	private Trigger buildTrigger(JobDetail detail, Trigger trigger, JobTriggerTO triggerTO) {
		
		TriggerBuilder<Trigger> builder = TriggerBuilder.newTrigger().forJob(detail).withIdentity(
				nvl(triggerTO.getTriggerName(), triggerTO.getOriginalTriggerName()),
				nvl(triggerTO.getTriggerGroup(), triggerTO.getOriginalTriggerGroup()));

		ScheduleBuilder<? extends Trigger> schedule = null;
		switch (triggerTO.getTriggerType()) {
			case CRON: {
				schedule = CronScheduleBuilder.cronSchedule(triggerTO.getCronExpression());
				if (null != triggerTO.getTimeZone()) {
					((CronScheduleBuilder)schedule).inTimeZone(triggerTO.getTimeZone());
				}
				switch (triggerTO.getMisfireInstruction()) {
					case CronTrigger.MISFIRE_INSTRUCTION_FIRE_ONCE_NOW:
						((CronScheduleBuilder)schedule).withMisfireHandlingInstructionFireAndProceed();
						break;
					case CronTrigger.MISFIRE_INSTRUCTION_IGNORE_MISFIRE_POLICY:
						((CronScheduleBuilder)schedule).withMisfireHandlingInstructionIgnoreMisfires();
						break;
					case CronTrigger.MISFIRE_INSTRUCTION_SMART_POLICY:
					case CronTrigger.MISFIRE_INSTRUCTION_DO_NOTHING:
					default:
						((CronScheduleBuilder)schedule).withMisfireHandlingInstructionDoNothing();
						break;
				}
				break;
			}
			case SIMPLE: {
				schedule = SimpleScheduleBuilder.simpleSchedule();
				if (triggerTO.getRepeatCount() < 0) {
					((SimpleScheduleBuilder)schedule).repeatForever();
				}
				else {
					((SimpleScheduleBuilder)schedule).withRepeatCount(triggerTO.getRepeatCount());
				}
				switch (triggerTO.getRepeatIntervalUnit()) {
					case MILLISECOND:
						((SimpleScheduleBuilder)schedule).withIntervalInMilliseconds(Long.valueOf(triggerTO.getRepeatInterval()));
						break;
					case SECOND:
						((SimpleScheduleBuilder)schedule).withIntervalInSeconds(Long.valueOf(triggerTO.getRepeatInterval()).intValue());
						break;
					case MINUTE:
						((SimpleScheduleBuilder)schedule).withIntervalInMinutes(Long.valueOf(triggerTO.getRepeatInterval()).intValue());
						break;
					case HOUR: 
						((SimpleScheduleBuilder)schedule).withIntervalInHours(Long.valueOf(triggerTO.getRepeatInterval()).intValue());
						break;
					case DAY: 
						((SimpleScheduleBuilder)schedule).withIntervalInHours(Long.valueOf(triggerTO.getRepeatInterval()).intValue()  * 24);
						break;
					case WEEK: 
						((SimpleScheduleBuilder)schedule).withIntervalInHours(Long.valueOf(triggerTO.getRepeatInterval()).intValue()  * 24 * 7);
						break;
					default:
						break;
				}
				switch (triggerTO.getMisfireInstruction()) {
					case SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_EXISTING_COUNT:
						((SimpleScheduleBuilder)schedule).withMisfireHandlingInstructionNextWithExistingCount();
						break;
					case SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_REMAINING_COUNT:
						((SimpleScheduleBuilder)schedule).withMisfireHandlingInstructionNextWithRemainingCount();
						break;
					case SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NOW_WITH_EXISTING_REPEAT_COUNT:
						((SimpleScheduleBuilder)schedule).withMisfireHandlingInstructionNowWithExistingCount();
						break;
					case SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NOW_WITH_REMAINING_REPEAT_COUNT:
						((SimpleScheduleBuilder)schedule).withMisfireHandlingInstructionNowWithRemainingCount();
						break;
					case SimpleTrigger.MISFIRE_INSTRUCTION_IGNORE_MISFIRE_POLICY:
						((SimpleScheduleBuilder)schedule).withMisfireHandlingInstructionIgnoreMisfires();
						break;
					case SimpleTrigger.MISFIRE_INSTRUCTION_SMART_POLICY:
					case SimpleTrigger.MISFIRE_INSTRUCTION_FIRE_NOW:
					default:
						((SimpleScheduleBuilder)schedule).withMisfireHandlingInstructionFireNow();
						break;
				}
				
			}
			case CALENDAR: {
				schedule = CalendarIntervalScheduleBuilder.calendarIntervalSchedule().withInterval(
						Long.valueOf(triggerTO.getRepeatInterval()).intValue(), triggerTO.getRepeatIntervalUnit());
				switch (triggerTO.getMisfireInstruction()) {
					case CalendarIntervalTrigger.MISFIRE_INSTRUCTION_FIRE_ONCE_NOW:
						((CalendarIntervalScheduleBuilder)schedule).withMisfireHandlingInstructionFireAndProceed();
						break;
					case CalendarIntervalTrigger.MISFIRE_INSTRUCTION_IGNORE_MISFIRE_POLICY:
						((CalendarIntervalScheduleBuilder)schedule).withMisfireHandlingInstructionIgnoreMisfires();
						break;
					case CalendarIntervalTrigger.MISFIRE_INSTRUCTION_SMART_POLICY:
					case CalendarIntervalTrigger.MISFIRE_INSTRUCTION_DO_NOTHING:
					default:
						((CalendarIntervalScheduleBuilder)schedule).withMisfireHandlingInstructionDoNothing();
						break;
				}
				break;
			}
			case DAILY: {
				schedule = DailyTimeIntervalScheduleBuilder.dailyTimeIntervalSchedule().withInterval(
						Long.valueOf(triggerTO.getRepeatInterval()).intValue(), triggerTO.getRepeatIntervalUnit())
						.withRepeatCount(triggerTO.getRepeatCount());
				switch (triggerTO.getMisfireInstruction()) {
					case DailyTimeIntervalTrigger.MISFIRE_INSTRUCTION_FIRE_ONCE_NOW:
						((DailyTimeIntervalScheduleBuilder)schedule).withMisfireHandlingInstructionFireAndProceed();
						break;
					case DailyTimeIntervalTrigger.MISFIRE_INSTRUCTION_IGNORE_MISFIRE_POLICY:
						((DailyTimeIntervalScheduleBuilder)schedule).withMisfireHandlingInstructionIgnoreMisfires();
						break;
					case DailyTimeIntervalTrigger.MISFIRE_INSTRUCTION_SMART_POLICY:
					case DailyTimeIntervalTrigger.MISFIRE_INSTRUCTION_DO_NOTHING:
					default:
						((DailyTimeIntervalScheduleBuilder)schedule).withMisfireHandlingInstructionDoNothing();
						break;
				}
				break;
			}
			default:
				break;
		}
		
		if (null != triggerTO.getJobData() && !triggerTO.getJobData().isEmpty()) {
			for (Entry<String, Object> entry : triggerTO.getJobData().entrySet()) {
				if (String.class.isAssignableFrom(entry.getValue().getClass())) {
					builder.usingJobData(entry.getKey(), (String)entry.getValue());
				} else if (Boolean.class.isAssignableFrom(entry.getValue().getClass())) {
					builder.usingJobData(entry.getKey(), (Boolean)entry.getValue());
				} else if (Integer.class.isAssignableFrom(entry.getValue().getClass())) {
					builder.usingJobData(entry.getKey(), (Integer)entry.getValue());
				} else if (Long.class.isAssignableFrom(entry.getValue().getClass())) {
					builder.usingJobData(entry.getKey(), (Long)entry.getValue());
				} else if (Double.class.isAssignableFrom(entry.getValue().getClass())) {
					builder.usingJobData(entry.getKey(), (Double)entry.getValue());
				} else if (Float.class.isAssignableFrom(entry.getValue().getClass())) {
					builder.usingJobData(entry.getKey(), (Float)entry.getValue());
				}
				if (LOGGER.isDebugEnabled())
					LOGGER.debug("jobDataEntry: " + entry.getKey() +", val: "+ entry.getValue() + ", class: " + entry.getValue().getClass());
			}
		}
//		else if (null != trigger && null != trigger.getJobDataMap() && !trigger.getJobDataMap().isEmpty()) {
//			builder.usingJobData(trigger.getJobDataMap());
//		}
		
		builder.withSchedule(schedule);
		
		if (null != triggerTO.getPriority()) {
			builder.withPriority(triggerTO.getPriority());
		}
		builder.withDescription(triggerTO.getTriggerDescription());
		if (null == triggerTO.getStartTime()) {
			builder.startAt(new Date());
		} else {
			builder.startAt(triggerTO.getStartTime());
		}
		return builder.build();
	}
}
