package de.chandre.admintool.filebrowser;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.AdminComponentImpl;
import de.chandre.admintool.core.component.MenuEntry;

/**
 * @author Andre
 */
@Component
public class AdminToolFilebrowserLoader
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolFilebrowserLoader.class);
	
	public static final String MAIN_PAGE_PATH = "filebrowser/content/filebrowser";
	
	
	@Autowired
	private AdminTool adminTool;
	
	@PostConstruct
	public void configureAdminTool()
	{
		LOGGER.info("adding Filebrowser view to admin tool");
		
		AdminComponent component = new AdminComponentImpl();
		component.setDisplayName("Filebrowser");
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("Filebrowser");
		mainMenu.setName("filebrowser");
		mainMenu.setTarget(MAIN_PAGE_PATH);
		component.setMainMenu(mainMenu);
		
		adminTool.addComponent(component);
	}
}
