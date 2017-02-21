package de.chandre.admintool.log4j2;

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
public class AdminToolLog4j2Loader extends AbstractAdminToolLoader
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolLog4j2Loader.class);
	
	@Autowired
	private AdminTool adminTool;
	
	@Autowired
	private AdminToolLog4j2Config config;
	
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
		component.setPosition(config.getComponentPosition());
		component.getSecurityRoles().addAll(config.getSecurityRoles());
		component.addAdditionalCSS("/static/admintool/css/log4j2.css", true);
		component.addAdditionalJS("/static/admintool/js/log4j2.js", true);
		component.setDisplayName("Log4j2 Console");
		
		String adminLtePrefix = getAdminLTEPrefixUri();
		boolean relative = !shouldCDNsUsed();
		//select 2 plugin
		component.addAdditionalJS(adminLtePrefix + "plugins/select2/select2.min.js", relative);
		component.addAdditionalCSS(adminLtePrefix + "plugins/select2/select2.min.css", relative);
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("Log4j2 Config");
		mainMenu.setName("log4j2");
		
		mainMenu.addSubmenuEntry(new MenuEntry("log4j2Loggers", "Log4j2 Loggers", "content/log4j2", config.getSecurityRoles()));
		mainMenu.addSubmenuEntry(new MenuEntry("log4j2Console", "Log4j2 Console", "content/log4j2Console", config.getSecurityRoles()));
		
		component.setMainMenu(mainMenu);
		
		adminTool.addComponent(component);
	}
}
