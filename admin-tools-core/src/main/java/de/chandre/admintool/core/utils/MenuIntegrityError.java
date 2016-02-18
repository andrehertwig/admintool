package de.chandre.admintool.core.utils;

import java.util.ArrayList;
import java.util.List;

import de.chandre.admintool.core.component.MenuEntry;

public class MenuIntegrityError {
	private List<MenuEntry> menuEntries = new ArrayList<>();
	private String error;
	
	public MenuIntegrityError(String error) {
		this.error = error;
	}
	public List<MenuEntry> getMenuEntries() {
		return menuEntries;
	}
	public void setMenuEntries(List<MenuEntry> menuEntries) {
		this.menuEntries = menuEntries;
	}
	public void addMenuEntry(MenuEntry menuEntry) {
		this.menuEntries.add(menuEntry);
	}
	public String getError() {
		return error;
	}
	public void setError(String error) {
		this.error = error;
	}
	public boolean isEmpty () {
		return menuEntries.isEmpty();
	}
	
}