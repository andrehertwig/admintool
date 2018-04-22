package de.chandre.admintool.jobs;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.quartz.DisallowConcurrentExecution;
import org.quartz.Job;
import org.quartz.JobExecutionContext;

/**
 * simple example of Quartz job
 * @author Andre
 *
 */
@DisallowConcurrentExecution
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
    	LOGGER.info("<div>I'm div</div>");
    	LOGGER.warn("Warning with umlauts äöü &");
    	LOGGER.info("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><root>I'm a XML</root>");
    	LOGGER.info("finished executing job: %s", jobExecutionContext.getJobDetail().getKey().getName());
    	
    	StackTraceElement[] trace = Thread.currentThread().getStackTrace();
    	if (null != trace) {
    		StringBuilder traceStr = new StringBuilder("Example Trace");
    		for (StackTraceElement stackTraceElement : trace) {
				traceStr.append("\n").append(stackTraceElement.toString());
			}
    		LOGGER.error(traceStr);
    	}
	}
}
