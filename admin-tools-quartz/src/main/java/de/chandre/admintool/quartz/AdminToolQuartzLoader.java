package de.chandre.admintool.quartz;

import java.util.HashSet;

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
public class AdminToolQuartzLoader extends AbstractAdminToolLoader
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolQuartzLoader.class);
	
	@Autowired
	private AdminTool adminTool;
	
	@Autowired
	private AdminToolQuartzConfig quartzConfig;
	
	@PostConstruct
	public void configureAdminTool()
	{
		if(!quartzConfig.isEnabled() || !coreConfig.isEnabled()) {
			LOGGER.info("admin tool's quartz management is deactivated");
			return;
		}
		LOGGER.info("adding Quartz view to admin tool");
		
		HashSet<String> allRoles = new HashSet<>();
		allRoles.addAll(quartzConfig.getSecurityRolesConfig());
		allRoles.addAll(quartzConfig.getSecurityRolesJobs());
		
		AdminComponent component = new AdminComponentImpl();
		component.getSecurityRoles().addAll(allRoles);
		component.setDisplayName("Quartz");
		component.addAdditionalCSS("/static/admintool/quartz/css/quartz.css", true);
		component.addAdditionalJS("/static/admintool/quartz/js/quartz.js", true);
		component.addAdditionalJS("/static/admintool/quartz/js/validator.min.js", true);
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("Quartz");
		mainMenu.setName("quartz");
		mainMenu.setTarget("content/quartz");
		mainMenu.setHide(quartzConfig.isHideMenuItem());
		mainMenu.setSecurityRoles(allRoles);
		component.setMainMenu(mainMenu);
		
		mainMenu.addSubmenuEntry(new MenuEntry("quartzconfig", "Quartz-Config", "quartz/content/quartzConfig",
				quartzConfig.getSecurityRolesConfig()));
		mainMenu.addSubmenuEntry(new MenuEntry("quartzjobs", "Quartz-Jobs", "quartz/content/quartzJobs",
				quartzConfig.getSecurityRolesJobs()));
		adminTool.addComponent(component);
	}
}
