package de.chandre.admintool.melody;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.AdminComponentImpl;
import de.chandre.admintool.core.component.MenuEntry;

/**
 * @author Andre
 */
@Component
public class AdminToolJminixLoader
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolJminixLoader.class);
	
	@Autowired
	private AdminTool adminTool;
	
	@Value("${adminTool.jminixPath:/jmx/}")
	private String jminixPath;
	
	@PostConstruct
	public AdminTool configureAdminTool()
	{
		LOGGER.info("adding JMX Console to admin tool");
		
		AdminComponent component = new AdminComponentImpl();
		component.setDisplayName("JMX Console");
		component.addAdditionalCSS("/static/admintool/jminix.css", true);
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("JminiX");
		mainMenu.setName("jminix");
		mainMenu.setTarget("content/jminix");
		mainMenu.addVariable("jminixPath", jminixPath);
		component.setMainMenu(mainMenu);
		
		adminTool.addComponent(component);
		return adminTool;
	}
}
