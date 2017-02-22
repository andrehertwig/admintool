package de.chandre.admintool.log4j2;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.logging.log4j.Level;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import de.chandre.admintool.core.AdminTool;

@Controller
@RequestMapping(AdminTool.ROOTCONTEXT + "/log4j2")
public class AdminLog4j2Controller
{
	private static final Log LOGGER = LogFactory.getLog(AdminLog4j2Controller.class);
	
	@Autowired
	private AdminToolLog4j2Util log4jUtil;
	
	@RequestMapping(value = "/changeLevel/{loggerName}/{level}", method = {RequestMethod.POST, RequestMethod.GET})
	@ResponseBody
	public String changeLevel(@PathVariable("loggerName") String loggerName, @PathVariable("level") String level,
			HttpServletRequest request)
	{
		return changeLevelParent(loggerName, level, false, request);
	}
	
	@RequestMapping(value = "/changeLevel/{loggerName}/{level}/parent/{parent}", method = {RequestMethod.POST, RequestMethod.GET})
	@ResponseBody
	public String changeLevelParent(@PathVariable("loggerName") String loggerName, @PathVariable("level") String level, 
			@PathVariable("parent") boolean parent, HttpServletRequest request)
	{
		LOGGER.info(String.format("change %s to %s (parent: %s)", loggerName, level, parent));
		try {
			log4jUtil.changeLogger(loggerName, level, parent);
		} catch (Exception e) {
			return "false";
		}
		if (loggerName.equals("ROOT") || parent) {
			return "reload";
		}
		return "true";
	}
	
	@RequestMapping(value = "/removeCustomLoggers", method = {RequestMethod.POST, RequestMethod.GET})
	@ResponseBody
	public String removeCustomLoggers() {
		LOGGER.info(String.format("removing custom loggers"));
		log4jUtil.removeCustomLoggers();
		return "reload";
	}
	
	@RequestMapping(value = "/getLevels", method = RequestMethod.GET)
	@ResponseBody
	public Collection<String> getLevels(HttpServletRequest request)
	{
		List<String> res = new ArrayList<>();
		for (Level level : log4jUtil.getLevels()) {
			res.add(level.name());
		}
		return res;
	}
	
	@RequestMapping(value = "/getLevelCss/{prefix}", method = RequestMethod.GET)
	@ResponseBody
	public Map<String, String> getLevelCssClass(@PathVariable("prefix") String prefix, HttpServletRequest request)
	{
		Map<String, String> css = new HashMap<>();
		for (Level level : log4jUtil.getLevels()) {
			css.put(level.name(), log4jUtil.getLoggerLevelCss(prefix, level));
		}
		return css;
	}
	
	@RequestMapping(value = "/initConsole", method = RequestMethod.POST)
	@ResponseBody
	public String initConsole(@RequestBody Log4j2ConsoleTO consoleTO, HttpServletRequest request)
	{
		try {
			HttpSession session = request.getSession(true);
			
			if (session.getAttribute(AdminToolLog4j2Util.SESSION_APPENDER_NAME) != null) {
				// there is already a output stream which should be closed first
				log4jUtil.closeOutputStreamAppender(String.class.cast(session.getAttribute(AdminToolLog4j2Util.SESSION_APPENDER_NAME)));
			}
			
			String name = log4jUtil.createOutputStreamAppender(consoleTO.getName(), consoleTO.getPattern(), consoleTO.getEncoding(), 
					consoleTO.getLoggerNames(), consoleTO.getLevel(), consoleTO.isRecursive(), consoleTO.isOverrideLogLevel());
			session.setAttribute(AdminToolLog4j2Util.SESSION_APPENDER_NAME, name);
			LOGGER.debug(String.format("log4j console initialized: %s, %s", consoleTO.getLevel(), consoleTO.getEncoding()));
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return "false";
		}
		return "true";
	}
	
	@RequestMapping(value = "/stopConsole", method = {RequestMethod.GET, RequestMethod.POST})
	@ResponseBody
	public String stopConsole(HttpServletRequest request)
	{
		try {
			HttpSession session = request.getSession(false);
			log4jUtil.closeOutputStreamAppender(String.class.cast(session.getAttribute(AdminToolLog4j2Util.SESSION_APPENDER_NAME)));
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return "false";
		}
		return "true";
	}
	
	@RequestMapping(value = {"/getConsoleContent", "/getConsoleContent/"}, method = {RequestMethod.GET, RequestMethod.POST})
	@ResponseBody
	public String getConsoleContent(HttpServletRequest request)
	{
		return getConsoleContent(null, request);
	}
	
	@RequestMapping(value = "/getConsoleContent/{encoding}", method = {RequestMethod.GET, RequestMethod.POST})
	@ResponseBody
	public String getConsoleContent(@PathVariable("encoding") String encoding, HttpServletRequest request)
	{
		try {
			HttpSession session = request.getSession(false);
			return log4jUtil.getStringOutput(String.class.cast(session.getAttribute(AdminToolLog4j2Util.SESSION_APPENDER_NAME)), encoding);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return e.getMessage();
		}
	}
}
