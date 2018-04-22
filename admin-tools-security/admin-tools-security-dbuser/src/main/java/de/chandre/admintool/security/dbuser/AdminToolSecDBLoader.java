package de.chandre.admintool.security.dbuser;

import java.util.HashSet;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.extras.java8time.dialect.Java8TimeDialect;

import de.chandre.admintool.core.AbstractAdminToolLoader;
import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.AdminComponentImpl;
import de.chandre.admintool.core.component.MenuEntry;

/**
 * 
 * @author André
 * @since 1.1.7
 *
 */
@Component
public class AdminToolSecDBLoader extends AbstractAdminToolLoader {
	private static final Log LOGGER = LogFactory.getLog(AdminToolSecDBLoader.class);
	
	@Autowired
	private AdminTool adminTool;
	
	@Autowired
	private AdminToolSecDBProperties config;
	
	@Autowired
	private TemplateEngine templateEngine;
	
	@PostConstruct
	public void configureAdminTool()
	{
		if(!config.isEnabled() || !coreConfig.isEnabled()) {
			LOGGER.info("admin tool's database user management is deactivated");
			return;
		}
		
		Java8TimeDialect timeDialect = new Java8TimeDialect();
		if (!templateEngine.getDialectsByPrefix().containsKey(timeDialect.getPrefix())) {
			LOGGER.info("adding Java8TimeDialect to thymeleaf");
			templateEngine.addDialect(timeDialect);
		}
		
		LOGGER.info("adding database user management view to admin tool");
		
		HashSet<String> allRoles = new HashSet<>();
		allRoles.addAll(config.getSecurityRolesClients());
		allRoles.addAll(config.getSecurityRolesGroups());
		allRoles.addAll(config.getSecurityRolesRoles());
		allRoles.addAll(config.getSecurityRolesUsers());
		
		AdminComponent component = new AdminComponentImpl.AdminComponentBuilder()
				.displayName("User-Management")
				.position(config.getPosition())
				.securityRoles(allRoles)
				.build();
		
		if (StringUtils.isEmpty(config.getValidatorCdnPath())) {
			component.addAdditionalJS("/static/admintool/security/js/validator.min.js", true);
		} else {
			component.addAdditionalJS(config.getValidatorCdnPath(), false);
		}
		
		String adminLtePrefix = getAdminLTEPrefixUri();
		boolean relative = !shouldCDNsUsed();
		//select 2 plugin
//		component.addAdditionalJS(getWebjarsPrefixUri() + "select2/" + config.getSelect2Version() + "/dist/js/select2.js", relative);
//		component.addAdditionalCSS(getWebjarsPrefixUri() + "select2/" + config.getSelect2Version() + "/dist/css/select2.min.css", relative);
		component.addAdditionalJS(adminLtePrefix + "plugins/select2/select2.min.js", relative);
		component.addAdditionalCSS(adminLtePrefix + "plugins/select2/select2.min.css", relative);
		//mustache js
		component.addAdditionalJS(getWebjarsPrefixUri() + "mustache/" + config.getMustacheVersion() + "/mustache.min.js", relative);
		
		component.addAdditionalJS("/static/admintool/js/validation-util.js", true);
		component.addAdditionalJS("/static/admintool/js/select2-util.js", true);
		
		component.addAdditionalCSS("/static/admintool/security/css/accessmanagement.css", true);
		component.addAdditionalJS("/static/admintool/security/js/accessmanagement.js", true);
		
		MenuEntry mainMenu = MenuEntry.builder()
				.displayName("Access Management")
				.name("accessmanagement")
				.target("security/content/notExisting")
				.securityRoles(allRoles)
				.resouceMessageKeySuffix(Constants.MSG_KEY_PREFIX + "accessManagement.displayname")
				.build();
		component.setMainMenu(mainMenu);
		
		//add users view
		mainMenu.addSubmenuEntry(MenuEntry.builder().name("accessmanagement/users").displayName("Users")
				.resouceMessageKeySuffix(Constants.MSG_KEY_PREFIX + "accessManagement.users.displayname")
				.target("security/content/users")
				.securityRoles(config.getSecurityRolesUsers())
				.additionalJS("/static/admintool/security/js/users.js", true)
				.build());
		
		//add user groups view
		mainMenu.addSubmenuEntry(MenuEntry.builder().name("accessmanagement/userGroups").displayName("UserGroups")
				.resouceMessageKeySuffix(Constants.MSG_KEY_PREFIX + "accessManagement.usergroups.displayname")
				.target("security/content/usergroups")
				.securityRoles(config.getSecurityRolesUsers())
				.additionalJS("/static/admintool/security/js/accessrelation.js", true)
				.additionalJS("/static/admintool/security/js/usergroups.js", true)
				.build());
		
		//add roles view
		mainMenu.addSubmenuEntry(MenuEntry.builder().name("accessmanagement/roles").displayName("Roles")
				.resouceMessageKeySuffix(Constants.MSG_KEY_PREFIX + "accessManagement.roles.displayname")
				.target("security/content/roles")
				.securityRoles(config.getSecurityRolesUsers())
				.additionalJS("/static/admintool/security/js/accessrelation.js", true)
				.build());
		
		if (config.getClients().isEnabled()) {
			//add clients view
			mainMenu.addSubmenuEntry(MenuEntry.builder().name("accessmanagement/clients").displayName("Clients")
					.resouceMessageKeySuffix(Constants.MSG_KEY_PREFIX + "accessManagement.clients.displayname")
					.target("security/content/clients")
					.securityRoles(config.getSecurityRolesUsers())
					.additionalJS("/static/admintool/security/js/accessrelation.js", true)
					.build());
		}
		
		//finally adding the component
		adminTool.addComponent(component);
	}
}
