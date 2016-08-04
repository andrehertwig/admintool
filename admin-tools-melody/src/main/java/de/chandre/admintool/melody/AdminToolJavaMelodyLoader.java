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
public class AdminToolJavaMelodyLoader extends AbstractAdminToolLoader
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolJavaMelodyLoader.class);
	
	@Autowired
	private AdminTool adminTool;
	
	@Value("${adminTool.melody.path:/monitoring}")
	private String melodyPath;
	
	@Value("#{'${admintool.melody.securityRoles:}'.split(';')}")
	private Set<String> securityRoles = new HashSet<>();
	
	@Value("${admintool.melody.componentPosition:}")
	private Integer componentPosition;
	
	@PostConstruct
	public void configureAdminTool()
	{
		if(!coreConfig.isEnabled()) {
			LOGGER.info("admin tool's javaMelody view integration is deactivated");
			return;
		}
		LOGGER.info("adding JavaMelody view to admin tool");
		LOGGER.debug(toString());
		
		AdminComponent component = new AdminComponentImpl();
		component.setPosition(componentPosition);
		component.getSecurityRoles().addAll(securityRoles);
		component.setDisplayName("JavaMelody");
		component.addAdditionalCSS("/static/admintool/melody.css", true);
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("JavaMelody");
		mainMenu.setName("javamelody");
		mainMenu.setTarget("content/melody");
		mainMenu.addVariable("melodyPath", melodyPath);
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
	 * @return the melodyPath
	 */
	public String getMelodyPath() {
		return melodyPath;
	}

	/**
	 * @param melodyPath the melodyPath to set
	 */
	public void setMelodyPath(String melodyPath) {
		this.melodyPath = melodyPath;
	}

	/**
	 * @return the componentPosition
	 */
	public Integer getComponentPosition() {
		return componentPosition;
	}

	/**
	 * @param componentPosition the componentPosition to set
	 */
	public void setComponentPosition(Integer componentPosition) {
		this.componentPosition = componentPosition;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AdminToolJavaMelodyLoader [melodyPath=").append(melodyPath).append(", securityRoles=")
				.append(securityRoles).append(", componentPosition=").append(componentPosition).append("]");
		return builder.toString();
	}
}
