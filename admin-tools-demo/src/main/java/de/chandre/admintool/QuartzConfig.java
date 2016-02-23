package de.chandre.admintool;

import java.util.List;

import org.quartz.CronTrigger;
import org.quartz.JobDetail;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.MethodInvokingFactoryBean;
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
		return createJobDetail(SimpleJob.class, null);
	}
	@Bean
	public JobDetailFactoryBean simpleJobDetail3() {
		return createJobDetail(SimpleJob.class, "Simple");
	}
	@Bean
	public JobDetailFactoryBean simpleJobDetail4() {
		return createJobDetail(InterruptableSampleJob.class, "Simple");
	}
	@Bean
	public JobDetailFactoryBean simpleCronJobDetail() {
		return createJobDetail(SimpleCronJob.class, "Cron");
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
		return createSimpleTrigger(jobDetail, "Simple", "Simple trigger 3", 5000L, 300000L);
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
	    return createCronTrigger(jobDetail, "Cron", "SimpleCronTrigger 1", "0 0/5 * 1/1 * ? *", 5000L);
	}
	@Bean(name="simpleCronTrigger2")
	public CronTriggerFactoryBean createSimpleCronTrigger2(@Qualifier("simpleCronJobDetail") JobDetail jobDetail) {
	    return createCronTrigger(jobDetail, "Cron", "SimpleCronTrigger 2", "0 0 0/1 1/1 * ? *", 5000L);
	}
	
	private static JobDetailFactoryBean createJobDetail(Class<?> jobClass, String group) {
		JobDetailFactoryBean detailFactoryBean = new JobDetailFactoryBean();
		detailFactoryBean.setJobClass(jobClass);
		if (null != group) {
			detailFactoryBean.setGroup(group);
		}
		return detailFactoryBean;
	}
	
	private static CronTriggerFactoryBean createCronTrigger(JobDetail jobDetail, String group, String description,
			String cronExpression, long startDelay) {
		CronTriggerFactoryBean factoryBean = new CronTriggerFactoryBean();
		factoryBean.setJobDetail(jobDetail);
		factoryBean.setGroup(group);
		factoryBean.setStartDelay(startDelay);
		factoryBean.setCronExpression(cronExpression);
		factoryBean.setDescription(description);
		factoryBean.setMisfireInstruction(CronTrigger.MISFIRE_INSTRUCTION_DO_NOTHING);
		return factoryBean;
	}
	
}
