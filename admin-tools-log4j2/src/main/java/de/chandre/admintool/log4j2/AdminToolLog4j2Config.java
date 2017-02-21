package de.chandre.admintool.log4j2;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import de.chandre.admintool.core.AdminToolConfig;

/**
 * @author Andre
 */
@Component("adminToolLog4j2Config")
public class AdminToolLog4j2Config implements AdminToolConfig
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolLog4j2Config.class);
	
	@Value("${adminTool.log4j2.enabled:true}")
	private boolean enabled;
	
	@Value("#{'${admintool.log4j2.securityRoles:}'.split(';')}")
	private Set<String> securityRoles = new HashSet<>();
	
	@Value("${admintool.log4j2.componentPosition:}")
	private Integer componentPosition;
	
	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public void setSecurityRoles(Set<String> securityRoles) {
		this.securityRoles = securityRoles;
	}

	/**
	 * @return the securityRoles
	 */
	public Set<String> getSecurityRoles() {
		return securityRoles;
	}

	/**
	 * @return the componentPosition
	 */
	public Integer getComponentPosition() {
		return componentPosition;
	}

	/**
	 * @param componentPosition the componentPosition to set
	 */
	public void setComponentPosition(Integer componentPosition) {
		this.componentPosition = componentPosition;
	}
	

	@Override
	public void printConfig() {
		LOGGER.info(toString());
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AdminToolLog4j2Config [enabled=").append(enabled).append(", securityRoles=")
				.append(securityRoles).append(", componentPosition=").append(componentPosition).append("]");
		return builder.toString();
	}
}
