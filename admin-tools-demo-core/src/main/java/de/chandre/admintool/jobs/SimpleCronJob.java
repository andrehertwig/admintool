package de.chandre.admintool.jobs;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.quartz.Job;
import org.quartz.JobExecutionContext;

/**
 * simple example of Quartz job
 * @author Andre
 *
 */
public class SimpleCronJob implements Job 
{
	private static final Logger LOGGER = LogManager.getFormatterLogger(SimpleCronJob.class);
	
	@Override
    public void execute(JobExecutionContext jobExecutionContext) {
		LOGGER.info("executing cron job: %s", jobExecutionContext.getJobDetail().getKey().getName());
		try {
			Thread.sleep(30000L);
		} catch (InterruptedException e) {
			LOGGER.error(e.getMessage(), e);
		}
		LOGGER.info("finished executing cron job: %s", jobExecutionContext.getJobDetail().getKey().getName());
	}
}
