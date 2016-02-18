package de.chandre.admintool.log4j2;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.MenuEntry;

/**
 * @author Andre
 */
@Component
public class AdminToolLog4j2Loader
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolLog4j2Loader.class);
	
	@Autowired
	private AdminTool adminTool;
	
	@PostConstruct
	public AdminTool configureAdminTool()
	{
		LOGGER.info("adding Log4j Console to admin tool");
		
		AdminComponent component = new AdminComponent();
		component.addAdditionalCSS("/static/admintool/css/log4j2.css");
		component.addAdditionalJS("/static/admintool/js/log4j2.js");
		component.setDisplayName("Log4j2 Console");
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("Log4j2 Console");
		mainMenu.setName("log4j2");
		mainMenu.setTarget("content/log4j2");
		component.setMainMenu(mainMenu);
		
		adminTool.addComponent(component);
		return adminTool;
	}
}
