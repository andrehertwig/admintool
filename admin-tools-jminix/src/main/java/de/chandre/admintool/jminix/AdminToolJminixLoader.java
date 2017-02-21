package de.chandre.admintool.jminix;

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
public class AdminToolJminixLoader extends AbstractAdminToolLoader
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolJminixLoader.class);
	
	@Autowired
	private AdminTool adminTool;
	
	@Autowired
	private AdminToolJminixConfig config;
	
	@PostConstruct
	public void configureAdminTool()
	{
		if(!coreConfig.isEnabled() || !config.isEnabled()) {
			LOGGER.info("admin tool's jminix browser integation is deactivated");
			return;
		}
		
		LOGGER.info("adding JMX Console to admin tool");
		LOGGER.debug(toString());
		
		AdminComponent component = new AdminComponentImpl();
		component.setPosition(config.getComponentPosition());
		component.getSecurityRoles().addAll(config.getSecurityRoles());
		component.setDisplayName("JMX Console");
		component.addAdditionalCSS("/static/admintool/jminix.css", true);
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("JminiX");
		mainMenu.setName("jminix");
		mainMenu.setTarget("content/jminix");
		mainMenu.addVariable("jminixPath", config.getJminixPath());
		component.setMainMenu(mainMenu);
		
		adminTool.addComponent(component);
	}
}
