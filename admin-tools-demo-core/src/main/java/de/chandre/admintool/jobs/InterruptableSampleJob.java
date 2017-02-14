package de.chandre.admintool.jobs;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.quartz.InterruptableJob;
import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.UnableToInterruptJobException;

/**
 * simple example of Quartz job
 * @author Andre
 *
 */
public class InterruptableSampleJob implements Job, InterruptableJob
{
	private static final Logger LOGGER = LogManager.getFormatterLogger(InterruptableSampleJob.class);
	
	private Thread thread;
	
    @Override
    public void execute(JobExecutionContext jobExecutionContext) {
    	LOGGER.info("start executing job: %s", jobExecutionContext.getJobDetail().getKey().getName());
    	thread = new Thread(new Runnable() {
			@Override
			public void run() {
				try {
					Thread.sleep(120000L);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		});
		thread.run();
    	LOGGER.info("finished executing job: %s", jobExecutionContext.getJobDetail().getKey().getName());
	}

	@Override
	public void interrupt() throws UnableToInterruptJobException {
		if (null != thread) {
			LOGGER.info("interrupting job");
			thread.interrupt();
		}
	}
}
