package de.chandre.admintool.core.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.springframework.util.StringUtils;

import de.chandre.admintool.core.component.MenuEntry;

/**
 * error class used by {@link AdminToolIntegrityUtil} if integrity errors have been found
 * 
 * @author Andre
 *
 */
public class MenuIntegrityError {
	
	private List<MenuEntry> menuEntries = new ArrayList<>();
	private String error;
	private String errorMsgKey;
	private MenuEntry reference;
	
	/**
	 * 
	 * @param error the error message
	 * @param reference reference menu entry object to which the error belongs to 
	 */
	public MenuIntegrityError(String error, String errorMsgKey, MenuEntry reference) {
		this.error = error;
		this.errorMsgKey = errorMsgKey;
		this.reference = reference;
	}
	
	/**
	 * the menu entries causing this error
	 * @return
	 */
	public List<MenuEntry> getMenuEntries() {
		return Collections.unmodifiableList(menuEntries);
	}
	
	/**
	 * 
	 * @param menuEntry
	 */
	public void addMenuEntry(MenuEntry menuEntry) {
		if (null != menuEntry) {
			this.menuEntries.add(menuEntry);
		}
	}
	
	/**
	 * @return the standard error message
	 */
	public String getError() {
		return error;
	}
	
	/**
	 * 
	 * @return a predefined key which can be used to load different message resources
	 * @since 1.0.3
	 */
	public String getErrorMsgKey() {
		return errorMsgKey;
	}

	/**
	 * 
	 * @return if errors on menu entries are available
	 */
	public boolean isEmpty () {
		return menuEntries.isEmpty() && StringUtils.isEmpty(error);
	}
	
	/**
	 * reference menu entry object to which the error belongs to 
	 * @return menuItem or null if error occured in component
	 * @since 1.0.3
	 */
	public MenuEntry getReference() {
		return reference;
	}
	
	/**
	 * reference menu entry object to which the error belongs to 
	 * @param reference
	 * @since 1.0.3
	 */
	void setReference(MenuEntry reference) {
		this.reference = reference;
	}
}