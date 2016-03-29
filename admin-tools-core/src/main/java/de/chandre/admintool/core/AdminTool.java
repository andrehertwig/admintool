package de.chandre.admintool.core;

import java.util.Set;

import de.chandre.admintool.core.component.AdminComponent;

public interface AdminTool {
	
	/**
	 * 
	 * @return
	 */
	public Set<AdminComponent> getComponents();
	
	/**
	 * 
	 * @param components
	 */
	public void addComponent(AdminComponent components);
	
	/**
	 * 
	 * @param components
	 */
	public void addComponents(Set<AdminComponent> components);
	
	/**
	 * 
	 * @return
	 */
	public Set<String> getGlobalJavaScripts();
	
	/**
	 * 
	 * @param globalJavaScript
	 */
	public void addGlobalJavaScript(String globalJavaScript);
	
	/**
	 * 
	 * @return
	 */
	public Set<String> getGlobalStyleSheets();
	
	/**
	 * 
	 * @param globalStyleSheet
	 */
	public void addGlobalStyleSheet(String globalStyleSheet);
}
