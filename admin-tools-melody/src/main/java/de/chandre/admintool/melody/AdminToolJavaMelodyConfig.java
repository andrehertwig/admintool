package de.chandre.admintool.melody;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import de.chandre.admintool.core.AdminToolConfig;

@Component("adminToolJavaMelodyConfig")
public class AdminToolJavaMelodyConfig implements AdminToolConfig {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolJavaMelodyConfig.class);
	
	@Value("${adminTool.melody.enabled:true}")
	private boolean enabled;
	
	@Value("${adminTool.melody.path:/monitoring}")
	private String melodyPath;
	
	@Value("#{'${admintool.melody.securityRoles:}'.split(';')}")
	private Set<String> securityRoles = new HashSet<>();
	
	@Value("${admintool.melody.componentPosition:}")
	private Integer componentPosition;

	
	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public String getMelodyPath() {
		return melodyPath;
	}

	public void setMelodyPath(String melodyPath) {
		this.melodyPath = melodyPath;
	}

	public Set<String> getSecurityRoles() {
		return securityRoles;
	}

	public void setSecurityRoles(Set<String> securityRoles) {
		this.securityRoles = securityRoles;
	}

	public Integer getComponentPosition() {
		return componentPosition;
	}

	public void setComponentPosition(Integer componentPosition) {
		this.componentPosition = componentPosition;
	}
	
	@Override
	public void printConfig() {
		LOGGER.debug(toString());
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AdminToolJavaMelodyConfig [enabled=").append(enabled).append(", melodyPath=").append(melodyPath)
				.append(", securityRoles=").append(securityRoles).append(", componentPosition=")
				.append(componentPosition).append("]");
		return builder.toString();
	}
}
