package de.chandre.admintool.log4j2;

import java.io.Serializable;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Transfer object for web console appender configuration 
 * @author Andre
 *
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.PROPERTY)
public class Log4j2ConsoleTO implements Serializable {
	private static final long serialVersionUID = 5978547675179942202L;
	
	private String name;
	private String pattern;
	private String encoding;
	private boolean recursive;
	private boolean overrideLogLevel;
	private String level;
	private Set<String> loggerNames;
	
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String getPattern() {
		return pattern;
	}
	public void setPattern(String pattern) {
		this.pattern = pattern;
	}
	public String getEncoding() {
		return encoding;
	}
	public void setEncoding(String encoding) {
		this.encoding = encoding;
	}
	public boolean isRecursive() {
		return recursive;
	}
	public void setRecursive(boolean recursive) {
		this.recursive = recursive;
	}
	public boolean isOverrideLogLevel() {
		return overrideLogLevel;
	}
	public void setOverrideLogLevel(boolean overrideLogLevel) {
		this.overrideLogLevel = overrideLogLevel;
	}
	public String getLevel() {
		return level;
	}
	public void setLevel(String level) {
		this.level = level;
	}
	public Set<String> getLoggerNames() {
		return loggerNames;
	}
	public void setLoggerNames(Set<String> loggerNames) {
		this.loggerNames = loggerNames;
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((encoding == null) ? 0 : encoding.hashCode());
		result = prime * result + ((level == null) ? 0 : level.hashCode());
		result = prime * result + ((loggerNames == null) ? 0 : loggerNames.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + (overrideLogLevel ? 1231 : 1237);
		result = prime * result + ((pattern == null) ? 0 : pattern.hashCode());
		result = prime * result + (recursive ? 1231 : 1237);
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
		Log4j2ConsoleTO other = (Log4j2ConsoleTO) obj;
		if (encoding == null) {
			if (other.encoding != null)
				return false;
		} else if (!encoding.equals(other.encoding))
			return false;
		if (level == null) {
			if (other.level != null)
				return false;
		} else if (!level.equals(other.level))
			return false;
		if (loggerNames == null) {
			if (other.loggerNames != null)
				return false;
		} else if (!loggerNames.equals(other.loggerNames))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (overrideLogLevel != other.overrideLogLevel)
			return false;
		if (pattern == null) {
			if (other.pattern != null)
				return false;
		} else if (!pattern.equals(other.pattern))
			return false;
		if (recursive != other.recursive)
			return false;
		return true;
	}
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("Log4j2ConsoleTO [name=").append(name).append(", pattern=").append(pattern).append(", encoding=")
				.append(encoding).append(", recursive=").append(recursive).append(", overrideLogLevel=")
				.append(overrideLogLevel).append(", level=").append(level).append(", loggerNames=").append(loggerNames)
				.append("]");
		return builder.toString();
	}
}
