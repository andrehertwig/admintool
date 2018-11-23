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
		if(!coreConfig.isEnabled() || !config.isEnabled()) {
			LOGGER.info("admin tool's log4j viewer deactivated");
			return;
		}
		LOGGER.info("adding Log4j Console to admin tool");
		LOGGER.debug(toString());
		
		AdminComponent component = new AdminComponentImpl();
		component.setPosition(config.getComponentPosition());
//		component.getSecurityRoles().addAll(config.getSecurityRoles());
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
		mainMenu.setResouceMessageKey(AdminTool.RESOURCE_MESSAGE_KEY_PREFIX + "log4j.displayName");
		
		mainMenu.addSubmenuEntry(MenuEntry.builder().name("log4j2/log4j2Loggers").displayName("Log4j2 Loggers")
				.resouceMessageKeySuffix("log4j.loggers.displayName").target("content/log4j2")
//				.securityRoles(config.getSecurityRoles())
				.build());
		mainMenu.addSubmenuEntry(MenuEntry.builder().name("log4j2/log4j2Console").displayName("Log4j2 Console")
				.resouceMessageKeySuffix("log4j.console.displayName").target("content/log4j2Console")
//				.securityRoles(config.getSecurityRoles())
				.build());
		
		component.setMainMenu(mainMenu);
		
		adminTool.addComponent(component);
	}
}
