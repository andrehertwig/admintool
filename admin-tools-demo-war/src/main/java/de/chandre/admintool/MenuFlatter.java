package de.chandre.admintool;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.AdminComponentImpl;
import de.chandre.admintool.core.component.MenuEntry;

@Component
@DependsOn({"adminToolPropertiesLoader", "adminToolDBBrowserLoader", "adminToolFilebrowserLoader",
	"adminToolJavaMelodyLoader", "adminToolJminixLoader", "adminToolLog4j2Loader", "adminToolQuartzLoader"})
public class MenuFlatter {
	
	@Autowired
	private AdminTool adminTool;
	
	@PostConstruct
	private void init() {
		
		AdminComponent component2 = new AdminComponentImpl();
		component2.setPosition(2);
		component2.setDisplayName("My-Component");
		component2.addSecurityRole("ROLE_ANONYMOUS");
		component2.addSecurityRole("ROLE_ADMIN");
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("My-Management");
		mainMenu.setName("myManagement");
		component2.setMainMenu(mainMenu);
		
		mainMenu.addSubmenuEntry(new MenuEntry("myDashboard", "MyDashboard", "content/myDashboard"));
		mainMenu.addSubmenuEntry(new MenuEntry("myDashboard2", "MyDashboard 2", "content/myDashboard2"));
		
		
		/*
		 * restructure the core components
		 */
		AdminComponent component3 = new AdminComponentImpl();
		component3.setPosition(4);
		component3.setDisplayName("App-Management");
		
		MenuEntry appMenu = new MenuEntry();
		appMenu.setDisplayName("App-Management");
		appMenu.setName("appManagement");
		for (AdminComponent rootComponent : adminTool.getComponents()) {
			component3.getSecurityRoles().addAll(rootComponent.getSecurityRoles());
			rootComponent.getMainMenu().getSecurityRoles().addAll(rootComponent.getSecurityRoles());
			
			//because of loosing the component should to copy additional css and js to its menu entries, otherwise each menu will load everything
			rootComponent.getMainMenu().getAdditionalCSS().putAll(rootComponent.getAdditionalCSS());
			rootComponent.getMainMenu().getAdditionalJS().putAll(rootComponent.getAdditionalJS());
			
			//tell all menu entries using the hierarchy for loading css and js
			rootComponent.getMainMenu().flattened().forEach(menu -> {
				menu.setUseCCSHierarchy(true);
				menu.setUseJSHierarchy(true);
			});
			
			appMenu.addSubmenuEntry(rootComponent.getMainMenu());
		}
		component3.setMainMenu(appMenu);
		
		//clear all components
		adminTool.getComponents().clear();

		//finally add the new components
		adminTool.getComponents().add(component2);
		adminTool.getComponents().add(component3);
	}

}
