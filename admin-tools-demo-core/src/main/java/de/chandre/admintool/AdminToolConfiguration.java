package de.chandre.admintool;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.AdminComponentImpl;
import de.chandre.admintool.core.component.MenuEntry;
import de.chandre.admintool.db.AdminToolDBBrowserExampleLoader;
import de.chandre.admintool.db.ExampleStatement;
import de.chandre.admintool.db.ExampleStatements;

@Component
public class AdminToolConfiguration {
	
	@Autowired
	private AdminTool adminTool;
	
	@Autowired
	private AdminToolDBBrowserExampleLoader exampleLoader;
	
	@PostConstruct
	public void configureAdminTool() {
		
		AdminComponent component = AdminComponentImpl.builder()
				.displayName("Demo-App-Component")
				.notificationTemplate("notifications/notification")
				.securityRole("ROLE_ANONYMOUS")
				.securityRole("ROLE_ADMIN")
				.position(1).build();

		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("Demo-App-Component");
		mainMenu.setName("demo");
		mainMenu.setTarget("");
		mainMenu.addVariable("message", "Welcome to your Dashboard");
		mainMenu.setResouceMessageKey(AdminTool.RESOURCE_MESSAGE_KEY_PREFIX + "demo.displayName");
		component.setMainMenu(mainMenu);
		
		//adding sub menu entries
		mainMenu.addSubmenuEntry(MenuEntry.builder().name("dashboard").displayName("Dashboard")
				.target("content/dashboard").resouceMessageKeySuffix("demo.dashboard.displayName").build());
		mainMenu.addSubmenuEntry(MenuEntry.builder().name("dashboard2").displayName("Dashboard 2")
				.target("content/dashboard2").resouceMessageKeySuffix("demo.dashboard2.displayName").build());
		
		//adding a new sub menu tree
		MenuEntry submenu = MenuEntry.builder().displayName("SubMulti")
				.resouceMessageKeySuffix("demo.subMulti.displayName")
				.submenuEntry(
						MenuEntry.builder().name("dashboard3").displayName("Dashboard 3").target("content/dashboard3")
								.resouceMessageKeySuffix("demo.subMulti.dashboard3.displayName").build())
				.build();
		submenu.addSubmenuEntry(
				MenuEntry.builder().name("dashboard4").displayName("Dashboard 4").target("content/dashboard4")
				.resouceMessageKeySuffix("demo.subMulti.dashboard4.displayName").build());
		mainMenu.addSubmenuEntry(submenu);
		
		//producing an error
		mainMenu.addSubmenuEntry(
				MenuEntry.builder().name("dashboard5").displayName("Dashboard 5").target("content/dashboard")
				.resouceMessageKeySuffix("demo.dashboard5.displayName").build());
		mainMenu.addSubmenuEntry(
				MenuEntry.builder().name("dashboard5").displayName("Dashboard 5").target("content/dashboard")
				.resouceMessageKeySuffix("demo.dashboard5.displayName").build());
		
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
