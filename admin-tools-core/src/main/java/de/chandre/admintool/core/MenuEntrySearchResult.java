package de.chandre.admintool.core;

import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.MenuEntry;

/**
 * 
 * @author Andre
 * @since 1.0.1
 */
public class MenuEntrySearchResult {
	
	public static final String NAME = "menuEntrySearchResult";
	
	private AdminComponent component;
	private MenuEntry menuEntry;
	
	public MenuEntrySearchResult(AdminComponent component, MenuEntry menuEntry) {
		super();
		this.component = component;
		this.menuEntry = menuEntry;
	}

	/**
	 * @return the component
	 */
	public AdminComponent getComponent() {
		return component;
	}
	
	/**
	 * @return the menuEntry
	 */
	public MenuEntry getMenuEntry() {
		return menuEntry;
	}
	
}
