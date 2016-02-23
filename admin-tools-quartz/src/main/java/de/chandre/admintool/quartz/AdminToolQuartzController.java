package de.chandre.admintool.quartz;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

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

	@RequestMapping(path = "/triggerJob", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public boolean triggerJob(@RequestParam("groupName") String groupName, @RequestParam("jobName") String jobName,
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
			HttpServletRequest request) {
		if (LOGGER.isDebugEnabled())
			LOGGER.debug(String.format("receiving interruptJob request for group: %s, job: %s", groupName, jobName));
		try {
			quarzService.interruptJob(groupName, jobName);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return false;
		}
		return true;
	}
}
