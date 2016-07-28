package de.chandre.admintool.filebrowser;

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
import de.chandre.admintool.fileviewer.AdminToolFileviewerConfig;

/**
 * @author Andre
 * @since 1.0.1
 */
@Component
public class AdminToolFilebrowserLoader extends AbstractAdminToolLoader
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolFilebrowserLoader.class);
	
	public static final String TARGET_FILEBROWSER = "filebrowser/content/filebrowser";
	public static final String TARGET_FILEVIEWER = "filebrowser/content/fileviewer";
	
	@Autowired
	private AdminToolFilebrowserConfig filebrowserConfig;
	
	@Autowired
	private AdminToolFileviewerConfig fileviewerConfig;
	
	@Autowired
	private AdminTool adminTool;
	
	@PostConstruct
	public void configureAdminTool()
	{
		if (!coreConfig.isEnabled() || !filebrowserConfig.isEnabled()) {
			LOGGER.info("admin tool's filebrowser is deactivated");
			return;
		}
		LOGGER.info("adding Filebrowser view to admin tool");
		
		AdminComponent component = new AdminComponentImpl();
		component.setDisplayName("Filebrowser");
		component.getSecurityRoles().addAll(filebrowserConfig.getSecurityRoles());
		
		if (fileviewerConfig.isEnabled()) {
			boolean relative = !shouldCDNsUsed();
			String commonPrefix = getWebjarsPrefixUri();
			component.addAdditionalJS("/static/admintool/filebrowser/js/fileviewer.js", true);
			
			String codeMirrorPrefix = commonPrefix + "codemirror/" + fileviewerConfig.getCodeMirrorVersion() + "/";
			
			component.addAdditionalJS(codeMirrorPrefix + "lib/codemirror.js", relative);
			component.addAdditionalJS(codeMirrorPrefix + "mode/meta.js", relative);
			
			if (null != fileviewerConfig.getCodeMirrorAddLibs()) {
				fileviewerConfig.getCodeMirrorAddLibs().forEach(libpath -> {
					component.addAdditionalJS(codeMirrorPrefix + libpath, relative);
				});
			}
			
			component.addAdditionalJS(codeMirrorPrefix + "addon/mode/loadmode.js", relative);
			
			component.addAdditionalJS(codeMirrorPrefix + "addon/hint/show-hint.js", relative);
			
			component.addAdditionalCSS(codeMirrorPrefix + "lib/codemirror.css", relative);
			component.addAdditionalCSS(codeMirrorPrefix + "addon/hint/show-hint.css", relative);
			
			component.addAdditionalCSS("/static/admintool/filebrowser/css/fileviewer.css", true);
		}
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("Filebrowser");
		mainMenu.setName("filebrowser");
		mainMenu.setTarget(TARGET_FILEBROWSER);
		mainMenu.setHide(filebrowserConfig.isHideMenuItem());
		
		component.setMainMenu(mainMenu);
		
		adminTool.addComponent(component);
	}
}
