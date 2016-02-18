package de.chandre.admintool.melody;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.MenuEntry;

/**
 * @author Andre
 */
@Component
public class AdminToolJavaMelodyLoader
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolJavaMelodyLoader.class);
	
	@Autowired
	private AdminTool adminTool;
	
	@Value("${adminTool.melodyPath:/monitoring}")
	private String melodyPath;
	
	@PostConstruct
	public AdminTool configureAdminTool()
	{
		LOGGER.info("adding JavaMelody view to admin tool");
		
		AdminComponent component = new AdminComponent();
		component.setDisplayName("JavaMelody");
		component.addAdditionalCSS("/static/admintool/melody.css");
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("JavaMelody");
		mainMenu.setName("javamelody");
		mainMenu.setTarget("content/melody");
		mainMenu.addVariable("melodyPath", melodyPath);
		component.setMainMenu(mainMenu);
		
		adminTool.addComponent(component);
		return adminTool;
	}
}
