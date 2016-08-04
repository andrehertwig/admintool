package de.chandre.admintool.log4j2;

import java.util.HashSet;
import java.util.Set;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import de.chandre.admintool.core.AbstractAdminToolLoader;
import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.AdminComponentImpl;
import de.chandre.admintool.core.component.MenuEntry;

/**
 * @author Andre
 */
@Component
public class AdminToolLog4j2Loader extends AbstractAdminToolLoader
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolLog4j2Loader.class);
	
	@Autowired
	private AdminTool adminTool;
	
	@Value("#{'${admintool.log4j2.securityRoles:}'.split(';')}")
	private Set<String> securityRoles = new HashSet<>();
	
	@Value("${admintool.log4j2.componentPosition:}")
	private Integer componentPosition;
	
	@PostConstruct
	public void configureAdminTool()
	{
		if(!coreConfig.isEnabled()) {
			LOGGER.info("admin tool's log4j viewer deactivated");
			return;
		}
		LOGGER.info("adding Log4j Console to admin tool");
		LOGGER.debug(toString());
		
		AdminComponent component = new AdminComponentImpl();
		component.setPosition(componentPosition);
		component.getSecurityRoles().addAll(securityRoles);
		component.addAdditionalCSS("/static/admintool/css/log4j2.css", true);
		component.addAdditionalJS("/static/admintool/js/log4j2.js", true);
		component.setDisplayName("Log4j2 Console");
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("Log4j2 Console");
		mainMenu.setName("log4j2");
		mainMenu.setTarget("content/log4j2");
		component.setMainMenu(mainMenu);
		
		adminTool.addComponent(component);
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

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AdminToolLog4j2Loader [securityRoles=").append(securityRoles).append(", componentPosition=")
				.append(componentPosition).append("]");
		return builder.toString();
	}
	
}
