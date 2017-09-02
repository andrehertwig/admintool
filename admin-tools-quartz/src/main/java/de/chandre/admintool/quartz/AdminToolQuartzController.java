package de.chandre.admintool.quartz;

import java.util.Arrays;
import java.util.Collection;
import java.util.TimeZone;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.quartz.DateBuilder.IntervalUnit;
import org.quartz.SchedulerException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.controller.AbstractAdminController;

/**
 * contoller for quartz jobs actions
 * 
 * @author Andre
 *
 */
@Controller
@RequestMapping(AdminTool.ROOTCONTEXT + "/quartz")
public class AdminToolQuartzController extends AbstractAdminController {
	private static final Log LOGGER = LogFactory.getLog(AdminToolQuartzService.class);

	@Autowired
	private AdminToolQuartzService quarzService;
	
	@RequestMapping(value = {"/quartzJobsInc",})
	public String getJobs(ModelMap model, HttpServletRequest request) {
		if(LOGGER.isTraceEnabled()) LOGGER.trace("serving quartz jobs include");
		addCommonContextVars(model, request);
		return AdminTool.ROOTCONTEXT_NAME + "/quartz/includes/quartzJobs.inc";
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

	@RequestMapping(path = "/executeJob/{jobGroup}/{jobName}", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public boolean executeJob(@PathVariable("jobGroup") String groupName, @PathVariable("jobName") String jobName,
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

	@RequestMapping(path = "/changeTriggerState/{jobGroup}/{jobName}", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public boolean changeJobState(@PathVariable("jobGroup") String groupName, @PathVariable("jobName") String jobName, HttpServletRequest request) {
		return changeJobState(groupName, jobName, null, null, request);
	}
	
	@RequestMapping(path = "/changeTriggerState/{jobGroup}/{jobName}/{triggerGroup}/{triggerName}", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public boolean changeJobState(@PathVariable("jobGroup") String groupName, @PathVariable("jobName") String jobName,
			@PathVariable("triggerGroup") String triggerGroup, @PathVariable("triggerName") String triggerName, HttpServletRequest request) {
		if (LOGGER.isDebugEnabled())
			LOGGER.debug(String.format("receiving changeJobState request for group: %s, job: %s, trigger: %s", groupName,
					jobName, triggerName));
		try {
			quarzService.changeTriggerState(groupName, jobName, triggerGroup, triggerName);
		} catch (Exception e) {
			return false;
		}
		return true;
	}
	
	@RequestMapping(path = "/interruptJob/{jobGroup}/{jobName}", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public boolean interruptJob(@PathVariable("jobGroup") String groupName, @PathVariable("jobName") String jobName, HttpServletRequest request) {
		return interruptJob(groupName, jobName, null, null, request);
	}

	@RequestMapping(path = "/interruptJob/{jobGroup}/{jobName}/{triggerGroup}/{triggerName}", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public boolean interruptJob(@PathVariable("jobGroup") String groupName, @PathVariable("jobName") String jobName,
			@PathVariable("triggerGroup") String triggerGroup, @PathVariable("triggerName") String triggerName, HttpServletRequest request) {
		if (LOGGER.isDebugEnabled())
			LOGGER.debug(String.format("receiving interruptJob request for group: %s, job: %s", groupName, jobName));
		try {
			if (null != triggerName) {
				quarzService.interruptTrigger(groupName, jobName, triggerGroup, triggerName);
			} else {
				quarzService.interruptJob(groupName, jobName);
			}
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return false;
		}
		return true;
	}
	
	
	@RequestMapping(path = "/removeTrigger/{jobGroup}/{jobName}", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public boolean removeTrigger(@PathVariable("jobGroup") String groupName, @PathVariable("jobName") String jobName, HttpServletRequest request) {
		return removeTrigger(groupName, jobName, null, null, request);
	}
	
	@RequestMapping(path = "/removeTrigger/{jobGroup}/{jobName}/{triggerGroup}/{triggerName}", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public boolean removeTrigger(@PathVariable("jobGroup") String groupName, @PathVariable("jobName") String jobName,
			@PathVariable("triggerGroup") String triggerGroup, @PathVariable("triggerName") String triggerName, HttpServletRequest request) {
		if (LOGGER.isDebugEnabled())
			LOGGER.debug(String.format("receiving removeTrigger request for group: %s, job: %s, trigger: %s", groupName,
					jobName, triggerName));
		try {
			return quarzService.removeTrigger(groupName, jobName, triggerGroup, triggerName);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return false;
		}
	}
	
	
	public JobTriggerTO getJobTriggerInfo(String groupName, String jobName, String triggerGroup, String triggerName, 
			String methodName, HttpServletRequest request) {
		if (LOGGER.isDebugEnabled())
			LOGGER.debug(String.format("receiving %s request for group: %s, job: %s, trigger: %s", methodName, groupName,
					jobName, triggerName));
		try {
			return quarzService.getTriggerInfo(groupName, jobName, triggerGroup, triggerName);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return null;
		}
	}
	
	@RequestMapping(path = "/getTriggerInfo/{jobGroup}/{jobName}", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public JobTriggerTO getJobInfo(@PathVariable("jobGroup") String jobGroup, @PathVariable("jobName") String jobName,
			HttpServletRequest request) {
		return getJobTriggerInfo(jobGroup, jobName, null, null, "getJobInfo", request);
	}
	
	@RequestMapping(path = "/getTriggerInfo/{jobGroup}/{jobName}/{triggerGroup}/{triggerName}", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public JobTriggerTO getTriggerInfo(@PathVariable("jobGroup") String jobGroup, @PathVariable("jobName") String jobName,
			@PathVariable("triggerGroup") String triggerGroup, @PathVariable("triggerName") String triggerName, HttpServletRequest request) {
		return getJobTriggerInfo(jobGroup, jobName, triggerGroup, triggerName, "getTriggerInfo", request);
	}
	
	@RequestMapping(path = "/getDefaultTimeZone", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public String getDefaultTimeZone(HttpServletRequest request) {
		if (LOGGER.isDebugEnabled()) LOGGER.debug("receiving getDefaultTimeZone request");
		return TimeZone.getDefault().getID();
	}
	
	@RequestMapping(path = "/getTimeZones", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public String[] getTimeZones(HttpServletRequest request) {
		if (LOGGER.isDebugEnabled()) LOGGER.debug("receiving getTimeZones request");
		return TimeZone.getAvailableIDs();
	}
	
	@RequestMapping(path = "/getInstructionSets", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public Collection<JobTriggerTO> getMisfireInstructions(HttpServletRequest request) {
		if (LOGGER.isDebugEnabled()) LOGGER.debug("receiving getMisfireInstructions request");
		return quarzService.getInstructionSets();
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
	
	@RequestMapping(path = "/changeTrigger", method = { RequestMethod.POST })
	@ResponseBody
	public boolean changeTrigger(@RequestBody JobTriggerTO triggerTO, HttpServletRequest request) {
		if (LOGGER.isDebugEnabled())
			LOGGER.debug(String.format("receiving changeTrigger request: %s", triggerTO));
		try {
			return quarzService.changeTrigger(triggerTO, false);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return false;
		}
	}
	
	@RequestMapping(path = "/addTrigger", method = { RequestMethod.POST })
	@ResponseBody
	public boolean addTrigger(@RequestBody JobTriggerTO triggerTO, HttpServletRequest request) {
		if (LOGGER.isDebugEnabled())
			LOGGER.debug(String.format("receiving addTrigger request: %s", triggerTO));
		try {
			return quarzService.changeTrigger(triggerTO, true);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return false;
		}
	}
	
	@RequestMapping(path = "/changeJob", method = { RequestMethod.POST })
	@ResponseBody
	public boolean changeJob(@RequestBody JobTriggerTO triggerTO, HttpServletRequest request) {
		if (LOGGER.isDebugEnabled())
			LOGGER.debug(String.format("receiving changeJob request: %s", triggerTO));
		try {
			return quarzService.changeJob(triggerTO);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return false;
		}
	}

	/**
	 * the quartz service from adminTool
	 * @return
	 */
	public AdminToolQuartzService getQuarzService() {
		return quarzService;
	}

	/**
	 * if you want to override some functionality, just inherit from {@link AdminToolQuartzServiceImpl},
	 *  override your methods and call this setter
	 * @param quarzService
	 */
	public void setQuarzService(AdminToolQuartzService quarzService) {
		this.quarzService = quarzService;
	}
	
}
