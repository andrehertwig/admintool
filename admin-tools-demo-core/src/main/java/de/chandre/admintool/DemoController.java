package de.chandre.admintool;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.AdminComponentImpl;
import de.chandre.admintool.core.component.MenuEntry;
import de.chandre.admintool.db.AdminToolDBBrowserExampleLoader;
import de.chandre.admintool.db.ExampleStatement;
import de.chandre.admintool.db.ExampleStatements;

/**
 * Demo controller
 * @author Andre
 *
 */
@Controller
public class DemoController
{
	@RequestMapping({"","/"})
	public String hello(ModelAndView model, HttpServletRequest request) {
		return "index";
	}
	
	@Autowired
	private AdminTool adminTool;
	
	@Autowired
	private AdminToolDBBrowserExampleLoader exampleLoader;
	
//	@Autowired
//	private AdminToolSecurityLoader secLoader;
	
	@PostConstruct
	private void createDemo()
	{
		//make the user view available
//		secLoader.addUsersMenu();
		
		
		AdminComponent component = new AdminComponentImpl();
		component.setDisplayName("Demo-App-Component");
		component.addNotificationTemplate("notifications/notification");
		component.addSecurityRole("ROLE_ANONYMOUS");
		component.addSecurityRole("ROLE_ADMIN");
		

		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("Demo-App-Component");
		mainMenu.setName("demo");
		mainMenu.setTarget("");
		mainMenu.addVariable("message", "Welcome to your Dashboard");
		component.setMainMenu(mainMenu);
		
		//adding sub menu entries
		mainMenu.addSubmenuEntry(new MenuEntry("dashboard", "Dashboard", "content/dashboard"));
		mainMenu.addSubmenuEntry(new MenuEntry("dashboard2", "Dashboard 2", "content/dashboard2"));
		
		//adding a new sub menu tree
		MenuEntry submenu = new MenuEntry("", "SubMulti", "");
		submenu.addSubmenuEntry(new MenuEntry("dashboard3", "Dashboard 3", "content/dashboard3"));
		submenu.addSubmenuEntry(new MenuEntry("dashboard4", "Dashboard 4", "content/dashboard4"));
		mainMenu.addSubmenuEntry(submenu);
		
		//producing an error
		mainMenu.addSubmenuEntry(new MenuEntry("dashboard5", "Dashboard 5", "content/dashboard"));
		mainMenu.addSubmenuEntry(new MenuEntry("dashboard5", "Dashboard 5", "content/dashboard"));
		
		adminTool.addComponent(component);
		
		AdminComponent defectComponent = new AdminComponentImpl();
		defectComponent.setDisplayName("DefectComponent");
		adminTool.addComponent(defectComponent);
		
		ExampleStatements statements = new ExampleStatements();
		statements.setDatasourceName("dataSource");
		statements.addExample("Common Tables", new ExampleStatement("SELECT * from LOGGING", "Select all from Logging table"));
		statements.addExample("Maintenance", new ExampleStatement("SELECT * from SCHEMA_VERSION", "Show Flyway migrations"));
		
		exampleLoader.addExamples(statements);
	}
}
