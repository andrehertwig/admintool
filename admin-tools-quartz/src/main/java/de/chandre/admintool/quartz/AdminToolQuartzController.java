package de.chandre.admintool.quartz;

import java.util.Arrays;
import java.util.Collection;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.quartz.DateBuilder.IntervalUnit;
import org.quartz.SchedulerException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import de.chandre.admintool.core.controller.AbstractAdminController;
import net.bull.javamelody.MonitoredWithSpring;

/**
 * contoller for quartz jobs actions
 * 
 * @author Andre
 *
 */
@Controller
@RequestMapping("/admintool/quartz")
@MonitoredWithSpring
public class AdminToolQuartzController {
	private static final Log LOGGER = LogFactory.getLog(AdminToolQuartzService.class);

	@Autowired
	private AdminToolQuartzService quarzService;
	
	@RequestMapping(value = {"/quartzJobsInc",})
	public String getJobs(ModelMap model, HttpServletRequest request) {
		
		if(LOGGER.isTraceEnabled()) LOGGER.trace("serving quartz jobs include");
		return "admintool/quartz/includes/quartzJobs.inc";
	}

	@RequestMapping(path = "/changeRunningState", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public boolean changeRunningState() {
		if (quarzService.isSchedulerRunning()) {
			quarzService.stopScheduler();
		} else {
			quarzService.startScheduler();
		}
		return quarzService.isSchedulerRunning();
	}

	@RequestMapping(path = "/executeJob", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public boolean executeJob(@RequestParam("groupName") String groupName, @RequestParam("jobName") String jobName,
			HttpServletRequest request) {
		if (LOGGER.isDebugEnabled())
			LOGGER.debug(String.format("receiving triggerJob request for group: %s, job: %s", groupName, jobName));
		try {
			quarzService.triggerJob(groupName, jobName);
		} catch (Exception e) {
			return false;
		}
		return true;
	}

	@RequestMapping(path = "/changeTriggerState", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public boolean changeJobState(@RequestParam("groupName") String groupName, @RequestParam("jobName") String jobName,
			@RequestParam(name = "triggerName", required = false) String triggerName, HttpServletRequest request) {
		if (LOGGER.isDebugEnabled())
			LOGGER.debug(String.format("receiving changeJobState request for group: %s, job: %s, trigger: %s", groupName,
					jobName, triggerName));
		try {
			quarzService.changeTriggerState(groupName, jobName, triggerName);
		} catch (Exception e) {
			return false;
		}
		return true;
	}

	@RequestMapping(path = "/interruptJob", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public boolean interruptJob(@RequestParam("groupName") String groupName, @RequestParam("jobName") String jobName,
			@RequestParam(name = "triggerName", required = false) String triggerName, HttpServletRequest request) {
		if (LOGGER.isDebugEnabled())
			LOGGER.debug(String.format("receiving interruptJob request for group: %s, job: %s", groupName, jobName));
		try {
			if (null != triggerName) {
				quarzService.interruptTrigger(groupName, jobName, triggerName);
			} else {
				quarzService.interruptJob(groupName, jobName);
			}
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return false;
		}
		return true;
	}
	
	@RequestMapping(path = "/removeTrigger", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public boolean removeTrigger(@RequestParam("groupName") String groupName, @RequestParam("jobName") String jobName,
			@RequestParam(name = "triggerName") String triggerName, HttpServletRequest request) {
		if (LOGGER.isDebugEnabled())
			LOGGER.debug(String.format("receiving removeTrigger request for group: %s, job: %s, trigger: %s", groupName,
					jobName, triggerName));
		try {
			return quarzService.removeTrigger(groupName, jobName, triggerName);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return false;
		}
	}
	
	@RequestMapping(path = "/getTriggerInfo", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public JobTriggerTA getTriggerInfo(@RequestParam("groupName") String groupName, @RequestParam("jobName") String jobName,
			@RequestParam(name = "triggerName", required=false) String triggerName, HttpServletRequest request) {
		if (LOGGER.isDebugEnabled())
			LOGGER.debug(String.format("receiving getTriggerInfo request for group: %s, job: %s, trigger: %s", groupName,
					jobName, triggerName));
		try {
			return quarzService.getTriggerInfo(groupName, jobName, triggerName);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return null;
		}
	}
	
	@RequestMapping(path = "/getMisfireInstructions", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public Collection<JobTriggerTA> getMisfireInstructions(HttpServletRequest request) {
		if (LOGGER.isDebugEnabled()) LOGGER.debug("receiving getMisfireInstructions request");
		return quarzService.getMisfireInstructionSets();
	}
	
	@RequestMapping(path = "/getCalendarNames", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public Collection<String> getCalendarNames(HttpServletRequest request) {
		if (LOGGER.isDebugEnabled()) LOGGER.debug("receiving getCalendarNames request");
		try {
			return quarzService.getCalendarNames();
		} catch (SchedulerException e) {
			LOGGER.error(e.getMessage(), e);
		}
		return null;
	}
	
	@RequestMapping(path = "/getIntervalUnits", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public Collection<IntervalUnit> getIntervalUnits(HttpServletRequest request) {
		if (LOGGER.isDebugEnabled()) LOGGER.debug("receiving getCalendarNames request");
		return Arrays.asList(IntervalUnit.values());
	}
	
}
