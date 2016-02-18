package de.chandre.admintool.log4j2;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.core.Logger;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.config.Configuration;
import org.apache.logging.log4j.core.config.LoggerConfig;
import org.apache.logging.log4j.spi.StandardLevel;
import org.springframework.stereotype.Service;

@Service("adminToolLog4j2Util")
public class AdminToolLog4j2Util 
{
	private static List<Level> LEVELS = new ArrayList<>(7);
	static {
		LEVELS.add(Level.OFF);
		LEVELS.add(Level.TRACE);
		LEVELS.add(Level.DEBUG);
		LEVELS.add(Level.INFO);
		LEVELS.add(Level.WARN);
		LEVELS.add(Level.ERROR);
		LEVELS.add(Level.FATAL);
	}
	
	private static final Comparator<Logger> LOGGER_COMP = new Comparator<Logger>() {
		@Override
		public int compare(Logger o1, Logger o2) {
			return o1.getName().compareTo(o2.getName());
		}
	};
	
	private List<String> customLoggers = new ArrayList<>();
	private List<String> customParentLoggers = new ArrayList<>();
	
	public Collection<Logger> getParentLoggers() {
		LoggerContext ctx = (LoggerContext) LogManager.getContext(false);
		List<Logger> loggers = new ArrayList<>(ctx.getLoggers());
		Map<String, Logger> parentMap = new HashMap<>();
		try {
			for (Logger logger : loggers) {
				if (null != logger.getParent() && parentMap.get(logger.getParent().getName()) == null) {
					parentMap.put(logger.getParent().getName(), logger.getParent());
				}
			}
			List<Logger> parents = new ArrayList<>(parentMap.values());
			Collections.sort(parents, LOGGER_COMP);
			return parents;
		} finally {
			loggers.clear();
			parentMap.clear();
		}
	}
	
	public Collection<Logger> getLoggers() {
		LoggerContext ctx = (LoggerContext) LogManager.getContext(false);
//		ctx.getLogger(LogManager.ROOT_LOGGER_NAME);
		List<Logger> loggers = new ArrayList<>(ctx.getLoggers());
		Collections.sort(loggers, LOGGER_COMP);
		return loggers;
	}
	
	public String getLoggerLevelCss(String prefix, Level level) {
		if (null == prefix) {
			prefix = "";
		} else {
			prefix += "-";
		}
		if (level.intLevel() == StandardLevel.TRACE.intLevel()) {
			return prefix + "info";
		}
		if (level.intLevel() == StandardLevel.DEBUG.intLevel()) {
			return prefix + "primary";		
		}
		if (level.intLevel() == StandardLevel.INFO.intLevel()) {
			return prefix + "success";
		}
		if (level.intLevel() == StandardLevel.WARN.intLevel()) {
			return prefix + "warning";
		}
		if (level.intLevel() == StandardLevel.ERROR.intLevel()) {
			return prefix + "danger";
		}
		if (level.intLevel() == StandardLevel.FATAL.intLevel()) {
			return prefix + "muted";
		}
		if (level.intLevel() == StandardLevel.OFF.intLevel()) {
			return prefix + "muted";
		}
		return "";
	}
	
	public Collection<Level> getLevels() {
		return LEVELS;
	}
	
	public void changeLogger(final String name, final String levelStr, boolean parent) throws IllegalArgumentException
	{
		Level level = Level.getLevel(levelStr);
		if (null == level || !LEVELS.contains(level)) {
			throw new IllegalArgumentException("wrong logger level: " + String.valueOf(levelStr));
		}
		if (null == name) {
			throw new IllegalArgumentException("logger name must not null");
		}
		String loggerName = name;
		if (name.equals("ROOT")) {
			loggerName = LogManager.ROOT_LOGGER_NAME;
		}
		LoggerContext ctx = (LoggerContext) LogManager.getContext(false);
		Configuration config = ctx.getConfiguration();
		LoggerConfig loggerConfig = config.getLoggerConfig(loggerName);
		if (null == loggerConfig) {
			throw new IllegalArgumentException("no logger config found for: " + String.valueOf(loggerName));
		}
		if (!loggerConfig.getName().equals(loggerName)) {
			LoggerConfig loggerConfigNew = new LoggerConfig();
			loggerConfigNew.setLevel(level);
		    config.addLogger(loggerName, loggerConfigNew);
		    if (parent) {
		    	customParentLoggers.add(loggerName);
		    } else {
		    	customLoggers.add(loggerName);
		    }
		}
		else {
			loggerConfig.setLevel(level);
		}
		ctx.updateLoggers();
	}
	
	public void removeCustomLoggers() throws IllegalArgumentException
	{
		if (customLoggers.isEmpty()) {
			return;
		}
		LoggerContext ctx = (LoggerContext) LogManager.getContext(false);
		Configuration config = ctx.getConfiguration();
		for (String logger : customLoggers) {
			config.removeLogger(logger);
		}
		ctx.updateLoggers();
		customLoggers.clear();
	}
	
}
