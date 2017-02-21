package de.chandre.admintool.jminix;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import de.chandre.admintool.core.AdminToolConfig;

@Component("adminToolJminixConfig")
public class AdminToolJminixConfig implements AdminToolConfig {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolJminixConfig.class);
	
	@Value("${adminTool.jminix.enabled:true}")
	private boolean enabled;
	
	@Value("${adminTool.jminix.path:/jmx/}")
	private String jminixPath;
	
	@Value("#{'${admintool.jminix.securityRoles:}'.split(';')}")
	private Set<String> securityRoles = new HashSet<>();
	
	@Value("${admintool.jminix.componentPosition:}")
	private Integer componentPosition;

	
	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public String getJminixPath() {
		return jminixPath;
	}

	public void setJminixPath(String jminixPath) {
		this.jminixPath = jminixPath;
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
		builder.append("AdminToolJminixConfig [enabled=").append(enabled).append(", jminixPath=").append(jminixPath)
				.append(", securityRoles=").append(securityRoles).append(", componentPosition=")
				.append(componentPosition).append("]");
		return builder.toString();
	}

	
}
