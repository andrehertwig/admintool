package de.chandre.admintool.core.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import de.chandre.admintool.core.component.MenuEntry;

public class MenuIntegrityError {
	
	private List<MenuEntry> menuEntries = new ArrayList<>();
	private String error;
	private MenuEntry reference;
	
	public MenuIntegrityError(String error, MenuEntry reference) {
		this.error = error;
		this.reference = reference;
	}
	public List<MenuEntry> getMenuEntries() {
		return Collections.unmodifiableList(menuEntries);
	}
	public void addMenuEntry(MenuEntry menuEntry) {
		if (null != menuEntry) {
			this.menuEntries.add(menuEntry);
		}
	}
	public String getError() {
		return error;
	}
	public boolean isEmpty () {
		return menuEntries.isEmpty();
	}
	
	/**
	 * reference object to which the error belongs to 
	 * @return menuItem or null if error occured in component
	 * @since 1.0.3
	 */
	public MenuEntry getReference() {
		return reference;
	}
	
	/**
	 * reference object to which the error belongs to 
	 * @param reference
	 * @since 1.0.3
	 */
	public void setReference(MenuEntry reference) {
		this.reference = reference;
	}
}