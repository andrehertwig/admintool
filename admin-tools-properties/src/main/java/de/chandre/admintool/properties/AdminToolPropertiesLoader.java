package de.chandre.admintool.properties;

import java.util.HashSet;
import java.util.Set;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import de.chandre.admintool.core.AbstractAdminToolLoader;
import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.AdminComponentImpl;
import de.chandre.admintool.core.component.MenuEntry;

/**
 * @author Andre
 */
@Component
public class AdminToolPropertiesLoader extends AbstractAdminToolLoader
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolPropertiesLoader.class);
	
	private static final String GIT_PROPS_PATH_DEFAULT = "classpath:git.properties";
	
	@Autowired
	private AdminTool adminTool;
	
	@Value("${admintool.properties.gitPropertiesPath:}")
	private String gitPropertiesPath;
	
	@Value("${admintool.properties.gitPropertiesEncoding:UTF-8}")
	private String gitPropertiesEncoding;
	
	@Value("#{'${admintool.properties.securityRoles:}'.split(';')}")
	private Set<String> securityRoles = new HashSet<>();
	
	@Value("${admintool.properties.componentPosition:}")
	private Integer componentPosition;
	
	@PostConstruct
	public void configureAdminTool()
	{
		if(!coreConfig.isEnabled()) {
			LOGGER.info("admin tool's properties visualizer is deactivated");
			return;
		}
		
		LOGGER.info("adding properties visualizer to admin tool");
		LOGGER.debug(toString());
		
		AdminComponent component = new AdminComponentImpl();
		component.setPosition(componentPosition);
		component.getSecurityRoles().addAll(securityRoles);
		component.setDisplayName("Properties");
		component.addAdditionalJS("/static/admintool/js/properties.js", true);
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("Properties");
		mainMenu.setName("properties");
		mainMenu.setTarget("properties/content/properties");
		component.setMainMenu(mainMenu);
		
		adminTool.addComponent(component);
	}
	
	/**
	 * @return the securityRoles
	 */
	public Set<String> getSecurityRoles() {
		return securityRoles;
	}

	/**
	 * @return the gitPropertiesPath
	 */
	public String getGitPropertiesPath() {
		if (StringUtils.isEmpty(gitPropertiesPath)) {
			return GIT_PROPS_PATH_DEFAULT;
		}
		return gitPropertiesPath;
	}

	/**
	 * @return the gitPropertiesEncoding
	 */
	public String getGitPropertiesEncoding() {
		return gitPropertiesEncoding;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AdminToolPropertiesLoader [gitPropertiesPath=").append(gitPropertiesPath)
				.append(", gitPropertiesEncoding=").append(gitPropertiesEncoding).append(", securityRoles=")
				.append(securityRoles).append(", componentPosition=").append(componentPosition).append("]");
		return builder.toString();
	}
	
	
}
