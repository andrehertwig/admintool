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
		component.addAdditionalCSS("/static/admintool/quartz/css/quartz.css");
		component.addAdditionalJS("/static/admintool/quartz/js/quartz.js");
		
		component.addAdditionalJS("https://cdn.jsdelivr.net/webjars/org.webjars.bower/adminlte/2.3.2/plugins/datepicker/bootstrap-datepicker.js");
		component.addAdditionalCSS("https://cdn.jsdelivr.net/webjars/org.webjars.bower/adminlte/2.3.2/plugins/datepicker/datepicker3.css");
		component.addAdditionalJS("https://cdn.jsdelivr.net/webjars/org.webjars.bower/adminlte/2.3.2/plugins/timepicker/bootstrap-timepicker.min.js");
		component.addAdditionalCSS("https://cdn.jsdelivr.net/webjars/org.webjars.bower/adminlte/2.3.2/plugins/timepicker/bootstrap-timepicker.min.css");
		
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("Quartz");
		mainMenu.setName("quartz");
		mainMenu.setTarget("content/quartz");
		component.setMainMenu(mainMenu);
		
		mainMenu.addSubmenuEntry(new MenuEntry("quartzconfig", "Quartz-Config", "quartz/content/quartzConfig"));
		mainMenu.addSubmenuEntry(new MenuEntry("quartzjobs", "Quartz-Jobs", "quartz/content/quartzJobs"));
		mainMenu.addSubmenuEntry(new MenuEntry("quartzjobs2", "Quartz-Jobs 2", "quartz/content/quartzJobs2"));
		mainMenu.addSubmenuEntry(new MenuEntry("quartzjobs3", "Quartz-Jobs 3", "quartz/content/quartzJobs3"));
		adminTool.addComponent(component);
		return adminTool;
	}
}
