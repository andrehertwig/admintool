package de.chandre.admintool.properties;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
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
	
	@Value("#{'${admintool.properties.securityRoles:}'.split(';')}")
	private Set<String> securityRoles = new HashSet<>();
	
	@Value("${admintool.properties.componentPosition:}")
	private Integer componentPosition;
	
	@Value("#{'${admintool.properties.addPropertiesPaths:}'.split(';')}")
	private List<String> addPropertiesPaths;
	
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

	@Override
	public void printConfig() {
		LOGGER.info(toString());
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AdminToolPropertiesConfig [enabled=").append(enabled).append(", gitPropertiesPath=")
				.append(gitPropertiesPath).append(", gitPropertiesEncoding=").append(gitPropertiesEncoding)
				.append(", securityRoles=").append(securityRoles).append(", componentPosition=")
				.append(componentPosition).append(", addPropertiesPaths=").append(addPropertiesPaths).append("]");
		return builder.toString();
	}

}
