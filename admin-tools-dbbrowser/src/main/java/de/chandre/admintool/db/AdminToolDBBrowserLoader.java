package de.chandre.admintool.db;

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
public class AdminToolDBBrowserLoader extends AbstractAdminToolLoader
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolDBBrowserLoader.class);
	
	@Autowired
	private AdminTool adminTool;
	
	@PostConstruct
	public AdminTool configureAdminTool()
	{
		LOGGER.info("adding database browser to admin tool");
		
		boolean relative = shouldCDNsUsed();
		String commonPrefix = getWebjarsPrefixUri();
		
		AdminComponent component = new AdminComponentImpl();
		component.setDisplayName("DB Browser");
		component.addAdditionalJS("/static/admintool/dbbrowser/js/dbbrowser.js", true);
		
		component.addAdditionalJS(commonPrefix + "codemirror/5.13.2/lib/codemirror.js", relative);
		component.addAdditionalJS(commonPrefix + "codemirror/5.13.2/mode/sql/sql.js", relative);
		component.addAdditionalJS(commonPrefix + "codemirror/5.13.2/addon/hint/show-hint.js", relative);
		component.addAdditionalJS(commonPrefix + "codemirror/5.13.2/addon/hint/sql-hint.js", relative);
		
		component.addAdditionalCSS(commonPrefix + "codemirror/5.13.2/lib/codemirror.css", relative);
		component.addAdditionalCSS(commonPrefix + "codemirror/5.13.2/addon/hint/show-hint.css", relative);
		
		component.addAdditionalCSS("/static/admintool/dbbrowser/css/dbbrowser.css", true);
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("DB Browser");
		mainMenu.setName("dbbrowser");
		mainMenu.setTarget("dbbrowser/content/dbbrowser");
		component.setMainMenu(mainMenu);
		
		adminTool.addComponent(component);
		return adminTool;
	}
}
