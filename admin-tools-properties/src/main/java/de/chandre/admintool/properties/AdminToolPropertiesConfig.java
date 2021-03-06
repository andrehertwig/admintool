package de.chandre.admintool.properties;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import de.chandre.admintool.core.AdminToolConfig;

/**
 * @author Andre
 */
@Component("adminToolPropertiesConfig")
public class AdminToolPropertiesConfig implements AdminToolConfig
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolPropertiesConfig.class);
	
	private static final String GIT_PROPS_PATH_DEFAULT = "classpath:git.properties";
	
	@Value("${adminTool.properties.enabled:true}")
	private boolean enabled;
	
	@Value("${admintool.properties.gitPropertiesPath:}")
	private String gitPropertiesPath;
	
	@Value("${admintool.properties.gitPropertiesEncoding:UTF-8}")
	private String gitPropertiesEncoding;
	
	@Value("${adminTool.properties.showEnvironmentProperties:true}")
	private boolean showEnvironmentProperties;
	
	@Value("#{'${admintool.properties.securityRoles:}'.split(';')}")
	private Set<String> securityRoles = new HashSet<>();
	
	@Value("${admintool.properties.componentPosition:}")
	private Integer componentPosition;
	
	@Value("#{'${admintool.properties.addPropertiesPaths:}'.split(';')}")
	private List<String> addPropertiesPaths;
	
	@Value("#{'${admintool.properties.pattern.blacklist:}'.split(';')}")
	private Set<String> patternBlacklist = new HashSet<>();
	
	private Set<Pattern> blacklistPatterns;
	
	@PostConstruct
	private void initPattern() {
		if (!CollectionUtils.isEmpty(patternBlacklist)) {
			blacklistPatterns = new HashSet<>(patternBlacklist.size());
			patternBlacklist.forEach(str -> blacklistPatterns.add(Pattern.compile(str, Pattern.CASE_INSENSITIVE)));
		} else {
			blacklistPatterns = Collections.emptySet();
		}
	}
	
	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}
	
	/**
	 * @return the securityRoles
	 */
	public Set<String> getSecurityRoles() {
		return securityRoles;
	}

	/**
	 * @return the gitPropertiesPath
	 */
	public String getGitPropertiesPath() {
		if (StringUtils.isEmpty(gitPropertiesPath)) {
			return GIT_PROPS_PATH_DEFAULT;
		}
		return gitPropertiesPath;
	}

	/**
	 * @return the gitPropertiesEncoding
	 */
	public String getGitPropertiesEncoding() {
		return gitPropertiesEncoding;
	}
	
	/**
	 * 
	 * @return
	 * @since 1.1.6
	 */
	public boolean isShowEnvironmentProperties() {
		return showEnvironmentProperties;
	}

	public Integer getComponentPosition() {
		return componentPosition;
	}

	public void setComponentPosition(Integer componentPosition) {
		this.componentPosition = componentPosition;
	}

	public void setGitPropertiesPath(String gitPropertiesPath) {
		this.gitPropertiesPath = gitPropertiesPath;
	}

	public void setGitPropertiesEncoding(String gitPropertiesEncoding) {
		this.gitPropertiesEncoding = gitPropertiesEncoding;
	}

	public void setSecurityRoles(Set<String> securityRoles) {
		this.securityRoles = securityRoles;
	}
	
	/**
	 * 
	 * @return
	 * @since 1.0.4
	 */
	public List<String> getAddPropertiesPaths() {
		return addPropertiesPaths;
	}

	/**
	 * 
	 * @param addPropertiesPaths
	 * @since 1.0.4
	 */
	public void setAddPropertiesPaths(List<String> addPropertiesPaths) {
		this.addPropertiesPaths = addPropertiesPaths;
	}
	
	public Set<String> getPatternBlacklist() {
		return patternBlacklist;
	}

	public void setPatternBlacklist(Set<String> patternBlacklist) {
		this.patternBlacklist = patternBlacklist;
		initPattern();
	}
	
	public Set<Pattern> getBlacklistPatterns() {
		return blacklistPatterns;
	}

	@Override
	public void printConfig() {
		LOGGER.info(toString());
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AdminToolPropertiesConfig [enabled=").append(enabled).append(", gitPropertiesPath=")
				.append(gitPropertiesPath).append(", gitPropertiesEncoding=").append(gitPropertiesEncoding)
				.append(", showEnvironmentProperties=").append(showEnvironmentProperties).append(", securityRoles=")
				.append(securityRoles).append(", componentPosition=").append(componentPosition)
				.append(", addPropertiesPaths=").append(addPropertiesPaths).append(", patternBlacklist=")
				.append(patternBlacklist).append("]");
		return builder.toString();
	}

}
