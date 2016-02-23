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
public class SimpleJob implements Job 
{
	private static final Logger LOGGER = LogManager.getFormatterLogger(SimpleJob.class);
	
    @Override
    public void execute(JobExecutionContext jobExecutionContext) {
    	LOGGER.info("start executing job: %s", jobExecutionContext.getJobDetail().getKey().getName());
    	try {
			Thread.sleep(20000L);
		} catch (InterruptedException e) {
			LOGGER.error(e.getMessage(), e);
		}
    	LOGGER.info("finished executing job: %s", jobExecutionContext.getJobDetail().getKey().getName());
	}
}
