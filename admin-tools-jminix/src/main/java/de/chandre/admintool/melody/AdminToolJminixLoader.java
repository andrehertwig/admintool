package de.chandre.admintool.melody;

import java.util.HashSet;
import java.util.Set;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
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
	
	@Value("${adminTool.jminix.path:/jmx/}")
	private String jminixPath;
	
	@Value("#{'${admintool.jminix.securityRoles:}'.split(';')}")
	private Set<String> securityRoles = new HashSet<>();
	
	@PostConstruct
	public void configureAdminTool()
	{
		if(!coreConfig.isEnabled()) {
			LOGGER.info("admin tool's jminix browser integation is deactivated");
			return;
		}
		
		LOGGER.info("adding JMX Console to admin tool");
		
		AdminComponent component = new AdminComponentImpl();
		component.getSecurityRoles().addAll(securityRoles);
		component.setDisplayName("JMX Console");
		component.addAdditionalCSS("/static/admintool/jminix.css", true);
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("JminiX");
		mainMenu.setName("jminix");
		mainMenu.setTarget("content/jminix");
		mainMenu.addVariable("jminixPath", jminixPath);
		component.setMainMenu(mainMenu);
		
		adminTool.addComponent(component);
	}
	
	/**
	 * @return the securityRoles
	 */
	public Set<String> getSecurityRoles() {
		return securityRoles;
	}
}
