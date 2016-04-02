package de.chandre.admintool.db;

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
public class AdminToolDBBrowserLoader
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolDBBrowserLoader.class);
	
	@Autowired
	private AdminTool adminTool;
	
	@PostConstruct
	public AdminTool configureAdminTool()
	{
		LOGGER.info("adding database browser to admin tool");
		
		AdminComponent component = new AdminComponent();
		component.setDisplayName("DB Browser");
		component.addAdditionalJS("/static/admintool/dbbrowser/js/dbbrowser.js");
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("DB Browser");
		mainMenu.setName("dbbrowser");
		mainMenu.setTarget("dbbrowser/content/dbbrowser");
		component.setMainMenu(mainMenu);
		
		adminTool.addComponent(component);
		return adminTool;
	}
}
