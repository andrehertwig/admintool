package de.chandre.admintool.quartz;

import java.util.HashSet;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

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
		component.setPosition(quartzConfig.getComponentPosition());
		component.getSecurityRoles().addAll(allRoles);
		component.setDisplayName("Quartz");
		
		if (StringUtils.isEmpty(quartzConfig.getValidatorCDNPath())) {
			component.addAdditionalJS("/static/admintool/quartz/js/validator.min.js", true);
		} else {
			component.addAdditionalJS(quartzConfig.getValidatorCDNPath(), false);
		}
		component.addAdditionalCSS("/static/admintool/quartz/css/quartz.css", true);
		component.addAdditionalJS("/static/admintool/quartz/js/quartz.js", true);
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("Quartz");
		mainMenu.setName("quartz");
		mainMenu.setTarget("content/quartz");
		mainMenu.setHide(quartzConfig.isHideMenuItem());
		mainMenu.setSecurityRoles(allRoles);
		mainMenu.setResouceMessageKey(AdminTool.RESOURCE_MESSAGE_KEY_PREFIX + "quartz.displayName");
		component.setMainMenu(mainMenu);
		
		mainMenu.addSubmenuEntry(MenuEntry.builder().name("quartzconfig").displayName("Quartz-Config")
				.resouceMessageKeySuffix("quartz.config.displayName").target("quartz/content/quartzConfig")
				.securityRoles(quartzConfig.getSecurityRolesConfig()).build());
		mainMenu.addSubmenuEntry(MenuEntry.builder().name("quartzjobs").displayName("Quartz-Jobs")
				.resouceMessageKeySuffix("quartz.jobs.displayName").target("quartz/content/quartzJobs")
				.securityRoles(quartzConfig.getSecurityRolesJobs()).build());
		adminTool.addComponent(component);
	}
}
