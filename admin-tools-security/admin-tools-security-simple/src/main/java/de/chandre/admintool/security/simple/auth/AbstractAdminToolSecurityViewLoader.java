package de.chandre.admintool.security.simple.auth;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import de.chandre.admintool.core.AbstractAdminToolLoader;
import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.AdminComponentImpl;
import de.chandre.admintool.core.component.MenuEntry;

/**
 * Abstract class to extend from to initialize the Security view.<br>
 * just call {@link #addUsersMenu()} while postConstruct
 * 
 * @author Andre
 * @since 1.1.5
 */
public class AbstractAdminToolSecurityViewLoader extends AbstractAdminToolLoader {

	private static final Log LOGGER = LogFactory.getLog(AbstractAdminToolSecurityViewLoader.class);

	@Autowired
	private AdminTool adminTool;
	
	@Value("${admintool.security.componentPosition:}")
	private Integer componentPosition;
	
	@Value("#{'${admintool.security.securityRoles.users:}'.split(';')}")
	private Set<String> securityRolesConfig = new HashSet<>();
	
	@Value("${admintool.security.mustacheVersion:2.3.0}")
	private String mustacheVersion;
	
	private AdminComponent component = new AdminComponentImpl();
	
	/**
	 * adds the users view to admin tool
	 */
	protected void addUsersMenu() {
		LOGGER.info("adding Authentication component");
		
		MenuEntry mainMenu = new MenuEntry("users", "Users", "security/content/users", securityRolesConfig);
		mainMenu.addAdditionalJS("/static/admintool/security/users.js", true);
		mainMenu.addAdditionalJS("/static/admintool/security/validator.min.js", true);
		mainMenu.setResouceMessageKey(AdminTool.RESOURCE_MESSAGE_KEY_PREFIX + "security.users.displayName");
		
		String adminLtePrefix = getAdminLTEPrefixUri();
		boolean relative = !shouldCDNsUsed();
		//select 2 plugin
		component.addAdditionalJS(adminLtePrefix + "plugins/select2/select2.min.js", relative);
		component.addAdditionalCSS(adminLtePrefix + "plugins/select2/select2.min.css", relative);
		//mustache js
		component.addAdditionalJS(getWebjarsPrefixUri() + "mustache/" + mustacheVersion + "/mustache.min.js", relative);
		
		
		component.setMainMenu(mainMenu);
		
		component.setPosition(componentPosition);
		component.getSecurityRoles().addAll(securityRolesConfig);
		component.setDisplayName("Authentication");
		
		adminTool.addComponent(component);
	}
}
