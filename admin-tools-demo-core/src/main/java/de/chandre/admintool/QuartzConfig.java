package de.chandre.admintool;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.quartz.CronTrigger;
import org.quartz.JobDetail;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.quartz.CronTriggerFactoryBean;
import org.springframework.scheduling.quartz.JobDetailFactoryBean;
import org.springframework.scheduling.quartz.SchedulerFactoryBean;
import org.springframework.scheduling.quartz.SimpleTriggerFactoryBean;

import de.chandre.admintool.jobs.InterruptableSampleJob;
import de.chandre.admintool.jobs.SimpleCronJob;
import de.chandre.admintool.jobs.SimpleJob;

@Configuration
public class QuartzConfig
{
	@Bean
	public SchedulerFactoryBean getSchedulerFactory(List<Trigger> triggers) {
		SchedulerFactoryBean bean = BeanUtils.instantiateClass(SchedulerFactoryBean.class);
		bean.setTriggers(triggers.toArray(new Trigger[triggers.size()]));
		return bean;
	}
	
	@Bean
	public JobDetailFactoryBean simpleJobDetail() {
		return createJobDetail(SimpleJob.class, null, "Just a Simple Job", null);
	}
	@Bean
	public JobDetailFactoryBean simpleJobDetail3() {
		return createJobDetail(SimpleJob.class, "Simple" , null, null);
	}
	@Bean
	public JobDetailFactoryBean simpleJobDetail4() {
		Map<String, Object> jobData = new HashMap<>();
		jobData.put("detail1", "anotherStringValue");
		jobData.put("detail2", Boolean.FALSE);
		jobData.put("detail3", Integer.valueOf(21));
		return createJobDetail(InterruptableSampleJob.class, "Simple", "Interruptable job to demonstrate the functionality", jobData);
	}
	@Bean
	public JobDetailFactoryBean simpleCronJobDetail() {
		Map<String, Object> jobData = new HashMap<>();
		jobData.put("detail1", "stringValue");
		jobData.put("detail2", Boolean.TRUE);
		jobData.put("detail3", Integer.valueOf(42));
		return createJobDetail(SimpleCronJob.class, "Cron", "Cron job example job detail", jobData);
	}
	
	@Bean(name="simpleJobTrigger1")
	public SimpleTriggerFactoryBean createSimpleTrigger(@Qualifier("simpleJobDetail") JobDetail jobDetail) {
		return createSimpleTrigger(jobDetail, null, "Simple trigger 1", 5000L, 60000L);
	}
	@Bean(name="simpleJobTrigger2")
	public SimpleTriggerFactoryBean createSimpleTrigger2(@Qualifier("simpleJobDetail") JobDetail jobDetail) {
		return createSimpleTrigger(jobDetail, null, "Simple trigger 2", 5000L, 85000L);
	}
	@Bean(name="simpleJobTrigger3")
	public SimpleTriggerFactoryBean createSimpleTrigger3(@Qualifier("simpleJobDetail3") JobDetail jobDetail) {
		return createSimpleTrigger(jobDetail, "Simple", "Simple trigger 3", 5000L, 150000L);
	}
	@Bean(name="simpleJobTrigger4")
	public SimpleTriggerFactoryBean createSimpleTrigger4(@Qualifier("simpleJobDetail4") JobDetail jobDetail) {
		return createSimpleTrigger(jobDetail, "Simple", null, 5000L, 300000L);
	}
	
	private static SimpleTriggerFactoryBean createSimpleTrigger(JobDetail jobDetail,String group, String description,
			long startDelay, long repeatInterval) {
		
		SimpleTriggerFactoryBean factoryBean = new SimpleTriggerFactoryBean();
		factoryBean.setJobDetail(jobDetail);
		
		if (null != group) {
			factoryBean.setGroup(group);
		}
		factoryBean.setStartDelay(startDelay);
		factoryBean.setRepeatInterval(repeatInterval);
		factoryBean.setDescription(description);
		factoryBean.setRepeatCount(SimpleTrigger.REPEAT_INDEFINITELY);
		
		// in case of misfire, ignore all missed triggers and continue :
		factoryBean.setMisfireInstruction(SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_REMAINING_COUNT);
		return factoryBean;
	}
	
	@Bean(name="simpleCronTrigger1")
	public CronTriggerFactoryBean createSimpleCronTrigger(@Qualifier("simpleCronJobDetail") JobDetail jobDetail) {
		
		Map<String, Object> jobData = new HashMap<>();
		jobData.put("key1", "stringValue");
		jobData.put("key2", Boolean.TRUE);
		jobData.put("key3", Integer.valueOf(42));
	    return createCronTrigger(jobDetail, "Cron", "SimpleCronTrigger 1", "0 0/5 * 1/1 * ? *", 5000L, jobData);
	}
	@Bean(name="simpleCronTrigger2")
	public CronTriggerFactoryBean createSimpleCronTrigger2(@Qualifier("simpleCronJobDetail") JobDetail jobDetail) {
	    return createCronTrigger(jobDetail, "Cron", null, "0 0 0/1 1/1 * ? *", 5000L, null);
	}
	
	private static JobDetailFactoryBean createJobDetail(Class<?> jobClass, String group, String description, 
			Map<String, ?> jobDataAsMap) {
		JobDetailFactoryBean detailFactoryBean = new JobDetailFactoryBean();
		detailFactoryBean.setJobClass(jobClass);
		if (null != group) {
			detailFactoryBean.setGroup(group);
		}
		detailFactoryBean.setDescription(description);
		if (null != jobDataAsMap) {
			detailFactoryBean.setJobDataAsMap(jobDataAsMap);
		}
		
		return detailFactoryBean;
	}
	
	private static CronTriggerFactoryBean createCronTrigger(JobDetail jobDetail, String group, String description,
			String cronExpression, long startDelay, Map<String, ?> jobDataAsMap) {
		CronTriggerFactoryBean factoryBean = new CronTriggerFactoryBean();
		factoryBean.setJobDetail(jobDetail);
		factoryBean.setGroup(group);
		factoryBean.setStartDelay(startDelay);
		factoryBean.setCronExpression(cronExpression);
		factoryBean.setDescription(description);
		factoryBean.setMisfireInstruction(CronTrigger.MISFIRE_INSTRUCTION_DO_NOTHING);
		if (null != jobDataAsMap) {
			factoryBean.setJobDataAsMap(jobDataAsMap);
		}
		return factoryBean;
	}
	
}
