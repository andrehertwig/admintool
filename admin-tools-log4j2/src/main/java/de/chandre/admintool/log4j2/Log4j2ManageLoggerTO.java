package de.chandre.admintool.log4j2;

import java.io.Serializable;
import java.util.List;

/**
 * 
 * @author Chandre
 * @since 1.1.6.4
 */
public class Log4j2ManageLoggerTO implements Serializable {
	private static final long serialVersionUID = 991894759701275970L;

	private String loggerName;
	
	private List<String> appenderNames;
	
	private String level;
	
	private boolean additivity;

	public String getLoggerName() {
		return loggerName;
	}

	public void setLoggerName(String loggerName) {
		this.loggerName = loggerName;
	}
	
	public List<String> getAppenderNames() {
		return appenderNames;
	}

	public void setAppenderNames(List<String> appenderNames) {
		this.appenderNames = appenderNames;
	}

	public String getLevel() {
		return level;
	}

	public void setLevel(String level) {
		this.level = level;
	}

	public boolean isAdditivity() {
		return additivity;
	}

	public void setAdditivity(boolean additivity) {
		this.additivity = additivity;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("Log4j2ManageLoggerTO [loggerName=").append(loggerName).append(", appenderNames=").append(appenderNames)
				.append(", level=").append(level).append(", additivity=").append(additivity).append("]");
		return builder.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (additivity ? 1231 : 1237);
		result = prime * result + ((appenderNames == null) ? 0 : appenderNames.hashCode());
		result = prime * result + ((level == null) ? 0 : level.hashCode());
		result = prime * result + ((loggerName == null) ? 0 : loggerName.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Log4j2ManageLoggerTO other = (Log4j2ManageLoggerTO) obj;
		if (additivity != other.additivity)
			return false;
		if (appenderNames == null) {
			if (other.appenderNames != null)
				return false;
		} else if (!appenderNames.equals(other.appenderNames))
			return false;
		if (level == null) {
			if (other.level != null)
				return false;
		} else if (!level.equals(other.level))
			return false;
		if (loggerName == null) {
			if (other.loggerName != null)
				return false;
		} else if (!loggerName.equals(other.loggerName))
			return false;
		return true;
	}
}
