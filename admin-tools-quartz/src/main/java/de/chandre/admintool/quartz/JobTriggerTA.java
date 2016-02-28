package de.chandre.admintool.quartz;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.quartz.DateBuilder.IntervalUnit;

@XmlRootElement
@XmlAccessorType(XmlAccessType.PROPERTY)
public class JobTriggerTA implements Serializable
{
	private static final long serialVersionUID = 1L;

	public enum TriggerType {
		CRON, SIMPLE, CALENDAR, DAILY, MUTABLE
	}
	
	private static List<TriggerType> types = Arrays.asList(TriggerType.values());
	
	private TriggerType type;
	
	private String jobGroup;
	private String jobName;
	private String description;
	
	private String triggerName;
	private String triggerGroup;
	private String triggerDescription;
	
	private String calendarName;
	
	private Date startTime;
	private Long startDelay;
	private Integer priority;
	private Integer misfireInstruction;
	private Map<String, Integer> misfireInstructions;
	
	//for cron
	private String cronExpression;
	private TimeZone timeZone;
	
	//for simple and daily
	private int repeatCount;
	//also for calendar
	private long repeatInterval;
	
	//for calendar 
	private IntervalUnit repeatIntervalUnit;
	
	private String originalJobGroup;
	private String originalJobName;
	private String originalTriggerName;
	private String originalTriggerGroup;

	public List<TriggerType> getTypes() {
		return types;
	}

	public TriggerType getType() {
		return type;
	}

	public void setType(TriggerType type) {
		this.type = type;
	}

	public String getJobGroup() {
		return jobGroup;
	}

	public void setJobGroup(String jobGroup) {
		this.jobGroup = jobGroup;
	}

	public String getJobName() {
		return jobName;
	}

	public void setJobName(String jobName) {
		this.jobName = jobName;
	}
	
	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getTriggerName() {
		return triggerName;
	}

	public void setTriggerName(String triggerName) {
		this.triggerName = triggerName;
	}
	
	public String getTriggerGroup() {
		return triggerGroup;
	}

	public void setTriggerGroup(String triggerGroup) {
		this.triggerGroup = triggerGroup;
	}

	public String getTriggerDescription() {
		return triggerDescription;
	}

	public void setTriggerDescription(String triggerDescription) {
		this.triggerDescription = triggerDescription;
	}

	public String getCalendarName() {
		return calendarName;
	}

	public void setCalendarName(String calendarName) {
		this.calendarName = calendarName;
	}

	public Date getStartTime() {
		return startTime;
	}

	public void setStartTime(Date startTime) {
		this.startTime = startTime;
	}

	public Long getStartDelay() {
		return startDelay;
	}

	public void setStartDelay(Long startDelay) {
		this.startDelay = startDelay;
	}

	public Integer getPriority() {
		return priority;
	}

	public void setPriority(Integer priority) {
		this.priority = priority;
	}

	public Integer getMisfireInstruction() {
		return misfireInstruction;
	}

	public void setMisfireInstruction(Integer misfireInstruction) {
		this.misfireInstruction = misfireInstruction;
	}

	public Map<String, Integer> getMisfireInstructions() {
		return misfireInstructions;
	}

	public void setMisfireInstructions(Map<String, Integer> misfireInstructions) {
		this.misfireInstructions = misfireInstructions;
	}
	
	public void addMisfireInstructions(String name, Integer misfireInstruction) {
		if (null == this.misfireInstructions) {
			this.misfireInstructions= new TreeMap<>();
		}
		this.misfireInstructions.put(name, misfireInstruction);
	}

	public String getCronExpression() {
		return cronExpression;
	}

	public void setCronExpression(String cronExpression) {
		this.cronExpression = cronExpression;
	}
	
	public TimeZone getTimeZone() {
		return timeZone;
	}

	public void setTimeZone(TimeZone timeZone) {
		this.timeZone = timeZone;
	}

	public int getRepeatCount() {
		return repeatCount;
	}

	public void setRepeatCount(int repeatCount) {
		this.repeatCount = repeatCount;
	}

	public long getRepeatInterval() {
		return repeatInterval;
	}

	public void setRepeatInterval(long repeatInterval) {
		this.repeatInterval = repeatInterval;
	}

	public IntervalUnit getRepeatIntervalUnit() {
		return repeatIntervalUnit;
	}

	public void setRepeatIntervalUnit(IntervalUnit repeatIntervalUnit) {
		this.repeatIntervalUnit = repeatIntervalUnit;
	}

	public String getOriginalJobGroup() {
		return originalJobGroup;
	}

	public void setOriginalJobGroup(String originalJobGroup) {
		this.originalJobGroup = originalJobGroup;
	}

	public String getOriginalJobName() {
		return originalJobName;
	}

	public void setOriginalJobName(String originalJobName) {
		this.originalJobName = originalJobName;
	}

	public String getOriginalTriggerName() {
		return originalTriggerName;
	}

	public void setOriginalTriggerName(String originalTriggerName) {
		this.originalTriggerName = originalTriggerName;
	}

	public String getOriginalTriggerGroup() {
		return originalTriggerGroup;
	}

	public void setOriginalTriggerGroup(String originalTriggerGroup) {
		this.originalTriggerGroup = originalTriggerGroup;
	}
}
