package de.chandre.admintool.quartz;

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
public class AdminToolQuartzLoader
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolQuartzLoader.class);
	
	@Autowired
	private AdminTool adminTool;
	
	@PostConstruct
	public AdminTool configureAdminTool()
	{
		LOGGER.info("adding Quartz view to admin tool");
		
		AdminComponent component = new AdminComponent();
		component.setDisplayName("Quartz");
		component.addAdditionalCSS("/static/admintool/css/quartz.css");
		component.addAdditionalJS("/static/admintool/js/quartz.js");
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("Quartz");
		mainMenu.setName("quartz");
		mainMenu.setTarget("content/quartz");
		component.setMainMenu(mainMenu);
		
		mainMenu.addSubmenuEntry(new MenuEntry("quartzconfig", "Quartz-Config", "content/quartzConfig"));
		mainMenu.addSubmenuEntry(new MenuEntry("quartzjobs", "Quartz-Jobs", "content/quartzJobs"));
		
		adminTool.addComponent(component);
		return adminTool;
	}
}
