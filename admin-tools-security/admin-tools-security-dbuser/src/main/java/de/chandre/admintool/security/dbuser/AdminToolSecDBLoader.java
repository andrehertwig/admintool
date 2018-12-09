package de.chandre.admintool.security.dbuser;

import java.util.Collection;
import java.util.stream.Collectors;

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
import de.chandre.admintool.core.sec.AdminToolRoles;
import de.chandre.admintool.security.dbuser.service.AdminToolSecDBRoleService;

/**
 * 
 * @author André
 * @since 1.2.0
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
	
	@Autowired
	private AdminToolSecDBRoleService roleService;
	
	@Autowired(required=false) 
	private Collection<AdminToolRoles> atRoles;
	
	@PostConstruct
	public void configureAdminTool()
	{
		if(!config.isEnabled() || !coreConfig.isEnabled()) {
			LOGGER.info("admin tool's database user management is deactivated");
			return;
		}
		LOGGER.info("adding database user management view to admin tool");
		
		Java8TimeDialect timeDialect = new Java8TimeDialect();
		if (!templateEngine.getDialectsByPrefix().containsKey(timeDialect.getPrefix())) {
			LOGGER.info("adding Java8TimeDialect to thymeleaf");
			templateEngine.addDialect(timeDialect);
		}
		
		if (config.isAddMissingRolesAutomatically()) {
			int roleInterfaceSize = atRoles != null ? atRoles.size() : 0;
			LOGGER.debug("found " + roleInterfaceSize + " interfaces with roles");
			if (roleInterfaceSize > 0) {
				roleService.addRolesIfNotExists(atRoles.stream().flatMap(roleI -> roleI.getRoles().stream()).collect(Collectors.toSet()));
			}
		}
		
		AdminComponent component = new AdminComponentImpl.AdminComponentBuilder()
				.displayName("User-Management")
				.position(config.getPosition())
				.build();
		
		if (StringUtils.isEmpty(config.getValidatorCdnPath())) {
			component.addAdditionalJS("/static/admintool/js/validator.min.js", true);
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
		
//		component.addAdditionalJS("/static/admintool/security/js/password.min.js", true);
//		component.addAdditionalCSS("/static/admintool/security/css/password.min.css", true);
		
		component.addAdditionalCSS("/static/admintool/security/css/accessmanagement.css", true);
		component.addAdditionalJS("/static/admintool/security/js/passwordgen.js", true);
		component.addAdditionalJS("/static/admintool/security/js/accessmanagement.js", true);
		
		MenuEntry mainMenu = MenuEntry.builder()
				.displayName("Access Management")
				.name("accessmanagement")
				.target("security/content/notExisting")
				.resouceMessageKeySuffix(Constants.MSG_KEY_PREFIX + "accessManagement.displayname")
				.build();
		component.setMainMenu(mainMenu);
		
		//add users view
		mainMenu.addSubmenuEntry(MenuEntry.builder().name("accessmanagement/users").displayName("Users")
				.resouceMessageKeySuffix(Constants.MSG_KEY_PREFIX + "accessManagement.users.displayname")
				.target("security/content/users")
				.additionalJS("/static/admintool/security/js/users.js", true)
				.build());
		
		//add user groups view
		mainMenu.addSubmenuEntry(MenuEntry.builder().name("accessmanagement/userGroups").displayName("UserGroups")
				.resouceMessageKeySuffix(Constants.MSG_KEY_PREFIX + "accessManagement.usergroups.displayname")
				.target("security/content/usergroups")
				.additionalJS("/static/admintool/security/js/accessrelation.js", true)
				.additionalJS("/static/admintool/security/js/usergroups.js", true)
				.build());
		
		//add roles view
		mainMenu.addSubmenuEntry(MenuEntry.builder().name("accessmanagement/roles").displayName("Roles")
				.resouceMessageKeySuffix(Constants.MSG_KEY_PREFIX + "accessManagement.roles.displayname")
				.target("security/content/roles")
				.additionalJS("/static/admintool/security/js/accessrelation.js", true)
				.additionalJS("/static/admintool/security/js/roles.js", true)
				.build());
		
		if (config.getClients().isEnabled()) {
			//add clients view
			mainMenu.addSubmenuEntry(MenuEntry.builder().name("accessmanagement/clients").displayName("Clients")
					.resouceMessageKeySuffix(Constants.MSG_KEY_PREFIX + "accessManagement.clients.displayname")
					.target("security/content/clients")
					.additionalJS("/static/admintool/security/js/accessrelation.js", true)
					.build());
		}
		
		//hidden pseudo target ... resolved via user controller -> addCommonContextVars(..)
		mainMenu.addSubmenuEntry(MenuEntry.builder().name("accessmanagement/user/profile").displayName("Profile")
				.hide(true)
				.target("security/content/profile")
				.additionalJS("/static/admintool/security/js/profile.js", true)
				.build());
		
		//hidden pseudo target ... resolved via user controller -> addCommonContextVars(..)
		mainMenu.addSubmenuEntry(MenuEntry.builder().name("accessmanagement/reset").displayName("Reset Password")
				.hide(true)
				.target("security/content/resetPassword")
				.additionalJS("/static/admintool/security/js/resetPassword.js", true)
				.build());
		
		//finally adding the component
		adminTool.addComponent(component);
	}
}
