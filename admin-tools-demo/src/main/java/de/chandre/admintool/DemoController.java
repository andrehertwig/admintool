package de.chandre.admintool;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.MenuEntry;
import net.bull.javamelody.MonitoredWithSpring;

/**
 * Demo controller
 * @author Andre
 *
 */
@Controller
@MonitoredWithSpring
public class DemoController
{
	@RequestMapping({"","/"})
	public String hello(ModelAndView model, HttpServletRequest request) {
		return "index";
	}
	
	@Autowired
	private AdminTool adminTool;
	
	@PostConstruct
	private void createDemo()
	{
		AdminComponent component = new AdminComponent();
		component.setDisplayName("Demo-App-Component");
		component.addNotificationTemplate("notifications/notification");
		
		MenuEntry mainMenu = new MenuEntry();
		mainMenu.setDisplayName("Demo-App-Component");
		mainMenu.setName("demo");
		mainMenu.setTarget("");
		mainMenu.addVariable("message", "Welcome to your Dashboard");
		component.setMainMenu(mainMenu);
		
		
		MenuEntry submenu = new MenuEntry("dashboard", "Dashboard", "content/dashboard");
		mainMenu.addSubmenuEntry(submenu);
		submenu = new MenuEntry("dashboard2", "Dashboard 2", "content/dashboard2");
		mainMenu.addSubmenuEntry(submenu);
		
		submenu = new MenuEntry("", "SubMulti", "");
		mainMenu.addSubmenuEntry(submenu);
		MenuEntry susubmenu = new MenuEntry("dashboard3", "Dashboard 3", "content/dashboard3");
		submenu.addSubmenuEntry(susubmenu);
		susubmenu = new MenuEntry("dashboard4", "Dashboard 4", "content/dashboard4");
		submenu.addSubmenuEntry(susubmenu);
		
		adminTool.addComponent(component);
	}
}
