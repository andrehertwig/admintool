package de.chandre.admintool.core;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.springframework.stereotype.Component;

import de.chandre.admintool.core.component.AdminComponent;

/**
 * the admin tool<br>
 * 
 * create a new {@link AdminComponent} and use {@link AdminTool#addComponent(AdminComponent)} to get it displayed
 * 
 * @author Andre
 *
 */
@Component("adminTool")
public class AdminToolImpl implements AdminTool
{
	private Set<AdminComponent> components = new TreeSet<>();
	
	private Map<String, Boolean> globalJavaScripts = new LinkedHashMap<>();
	private Map<String, Boolean> globalStyleSheets = new LinkedHashMap<>();
	

	/**
	 * @return the components
	 */
	public Set<AdminComponent> getComponents() {
		return components;
	}
	
	/**
	 * @param components the components to set
	 */
	public void addComponent(AdminComponent components) {
		this.components.add(components);
	}
	

	/**
	 * @param components the components to set
	 */
	public void addComponents(Set<AdminComponent> components) {
		this.components.addAll(components);
	}

	public void setComponents(Set<AdminComponent> components) {
		this.components =components;
	}
	
	public Map<String, Boolean> getGlobalJavaScripts() {
		return globalJavaScripts;
	}

	public void setGlobalJavaScripts(Map<String, Boolean> globalJavaScripts) {
		this.globalJavaScripts = globalJavaScripts;
	}
	
	public void addGlobalJavaScript(String globalJavaScript, boolean local) {
		this.globalJavaScripts.put(globalJavaScript, local);
	}

	public Map<String, Boolean> getGlobalStyleSheets() {
		return globalStyleSheets;
	}

	public void setGlobalStyleSheet(Map<String, Boolean> globalStyleSheets) {
		this.globalStyleSheets = globalStyleSheets;
	}
	
	public void addGlobalStyleSheet(String globalStyleSheet, boolean local) {
		this.globalStyleSheets.put(globalStyleSheet, local);
	}
}
