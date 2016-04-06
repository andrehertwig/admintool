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
	
	@Autowired
	private AdminToolDBBrowserConfig config;
	
	@PostConstruct
	public AdminTool configureAdminTool()
	{
		if(!config.isEnabled()) {
			LOGGER.info(" admin tool's database browser deactivated");
			return adminTool;
		}
		LOGGER.info("adding database browser to admin tool");
		
		boolean relative = shouldCDNsUsed();
		String commonPrefix = getWebjarsPrefixUri();
		
		AdminComponent component = new AdminComponentImpl();
		component.setDisplayName("DB Browser");
		component.addAdditionalJS("/static/admintool/dbbrowser/js/dbbrowser.js", true);
		
		String codeMirrorPrefix = commonPrefix + "codemirror/" + config.getCodeMirrorVersion() + "/";
		
		component.addAdditionalJS(codeMirrorPrefix + "lib/codemirror.js", relative);
		component.addAdditionalJS(codeMirrorPrefix + "mode/sql/sql.js", relative);
		component.addAdditionalJS(codeMirrorPrefix + "addon/edit/matchbrackets.js", relative);
		
		component.addAdditionalJS(codeMirrorPrefix + "addon/hint/show-hint.js", relative);
		component.addAdditionalJS(codeMirrorPrefix + "addon/hint/sql-hint.js", relative);
		
		component.addAdditionalCSS(codeMirrorPrefix + "lib/codemirror.css", relative);
		component.addAdditionalCSS(codeMirrorPrefix + "addon/hint/show-hint.css", relative);
		
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
