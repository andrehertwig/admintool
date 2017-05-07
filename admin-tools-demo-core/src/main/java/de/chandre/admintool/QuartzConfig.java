package de.chandre.admintool;

import java.util.HashMap;
import java.util.Map;

import org.quartz.JobDetail;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.quartz.CronTriggerFactoryBean;
import org.springframework.scheduling.quartz.JobDetailFactoryBean;
import org.springframework.scheduling.quartz.SimpleTriggerFactoryBean;

import de.chandre.admintool.jobs.InterruptableSampleJob;
import de.chandre.admintool.jobs.SimpleCronJob;
import de.chandre.admintool.jobs.SimpleJob;
import de.chandre.quartz.spring.QuartzUtils;

/**
 * Using https://github.com/andrehertwig/spring-boot-starter-quartz for scheduler config
 * @author André Hertwig
 */
@Configuration
public class QuartzConfig
{
	@Bean
	public JobDetailFactoryBean simpleJobDetail() {
		return QuartzUtils.createJobDetail(SimpleJob.class, null, null, "Just a Simple Job", null);
	}
	@Bean
	public JobDetailFactoryBean simpleJobDetail3() {
		return QuartzUtils.createJobDetail(SimpleJob.class, null, "Simple" , null, null);
	}
	@Bean
	public JobDetailFactoryBean simpleJobDetail4() {
		Map<String, Object> jobData = new HashMap<>();
		jobData.put("detail1", "anotherStringValue");
		jobData.put("detail2", Boolean.FALSE);
		jobData.put("detail3", Integer.valueOf(21));
		return QuartzUtils.createJobDetail(InterruptableSampleJob.class, null, "Simple", "Interruptable job to demonstrate the functionality", jobData);
	}
	@Bean
	public JobDetailFactoryBean simpleCronJobDetail() {
		Map<String, Object> jobData = new HashMap<>();
		jobData.put("detail1", "stringValue");
		jobData.put("detail2", Boolean.TRUE);
		jobData.put("detail3", Integer.valueOf(42));
		return QuartzUtils.createJobDetail(SimpleCronJob.class, null, "Cron", "Cron job example job detail", jobData);
	}
	
	@Bean(name="simpleJobTrigger1")
	public SimpleTriggerFactoryBean createSimpleTrigger(@Qualifier("simpleJobDetail") JobDetail jobDetail) {
		return QuartzUtils.createSimpleTrigger(jobDetail, null, null, "Simple trigger 1", 5000L, 60000L, null);
	}
	@Bean(name="simpleJobTrigger2")
	public SimpleTriggerFactoryBean createSimpleTrigger2(@Qualifier("simpleJobDetail") JobDetail jobDetail) {
		return QuartzUtils.createSimpleTrigger(jobDetail, null, null, "Simple trigger 2", 5000L, 85000L, null);
	}
	@Bean(name="simpleJobTrigger3")
	public SimpleTriggerFactoryBean createSimpleTrigger3(@Qualifier("simpleJobDetail3") JobDetail jobDetail) {
		return QuartzUtils.createSimpleTrigger(jobDetail, null, "Simple", "Simple trigger 3", 5000L, 150000L, null);
	}
	@Bean(name="simpleJobTrigger4")
	public SimpleTriggerFactoryBean createSimpleTrigger4(@Qualifier("simpleJobDetail4") JobDetail jobDetail) {
		return QuartzUtils.createSimpleTrigger(jobDetail, null, "Simple", null, 5000L, 300000L, null);
	}
	
	@Bean(name="simpleCronTrigger1")
	public CronTriggerFactoryBean createSimpleCronTrigger(@Qualifier("simpleCronJobDetail") JobDetail jobDetail) {
		
		Map<String, Object> jobData = new HashMap<>();
		jobData.put("key1", "stringValue");
		jobData.put("key2", Boolean.TRUE);
		jobData.put("key3", Integer.valueOf(42));
	    return QuartzUtils.createCronTrigger(jobDetail, null, "Cron", "SimpleCronTrigger 1", "0 0/5 * 1/1 * ? *", 5000L, jobData);
	}
	@Bean(name="simpleCronTrigger2")
	public CronTriggerFactoryBean createSimpleCronTrigger2(@Qualifier("simpleCronJobDetail") JobDetail jobDetail) {
	    return QuartzUtils.createCronTrigger(jobDetail, null, "Cron", null, "0 0 0/1 1/1 * ? *", 5000L, null);
	}
	
}
