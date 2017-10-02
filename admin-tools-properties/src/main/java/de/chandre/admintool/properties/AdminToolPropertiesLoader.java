package de.chandre.admintool.properties;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
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
public class AdminToolPropertiesLoader extends AbstractAdminToolLoader
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolPropertiesLoader.class);
	
	@Autowired
	private AdminTool adminTool;

	@Autowired
	private AdminToolPropertiesConfig config;
	
	@PostConstruct
	public void configureAdminTool()
	{
		if(!coreConfig.isEnabled() || !config.isEnabled()) {
			LOGGER.info("admin tool's properties visualizer is deactivated");
			return;
		}
		
		LOGGER.info("adding properties visualizer to admin tool");
		
		AdminComponent component = new AdminComponentImpl();
		component.setPosition(config.getComponentPosition());
		component.getSecurityRoles().addAll(config.getSecurityRoles());
		component.setDisplayName("Properties");
		component.addAdditionalJS("/static/admintool/js/properties.js", true);
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("Properties");
		mainMenu.setName("properties");
		mainMenu.setTarget("properties/content/properties");
		mainMenu.setResouceMessageKey(AdminTool.RESOURCE_MESSAGE_KEY_PREFIX + "properties.displayName");
		component.setMainMenu(mainMenu);
		
		adminTool.addComponent(component);
	}
	
	
}
