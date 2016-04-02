package de.chandre.admintool.core;

import java.util.LinkedHashSet;
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
	
	private Set<String> globalJavaScripts = new LinkedHashSet<>();
	private Set<String> globalStyleSheets = new LinkedHashSet<>();
	

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
	
	public Set<String> getGlobalJavaScripts() {
		return globalJavaScripts;
	}

	public void setGlobalJavaScripts(Set<String> globalJavaScripts) {
		this.globalJavaScripts = globalJavaScripts;
	}
	
	public void addGlobalJavaScript(String globalJavaScript) {
		this.globalJavaScripts.add(globalJavaScript);
	}

	public Set<String> getGlobalStyleSheets() {
		return globalStyleSheets;
	}

	public void setGlobalStyleSheet(Set<String> globalStyleSheets) {
		this.globalStyleSheets = globalStyleSheets;
	}
	
	public void addGlobalStyleSheet(String globalStyleSheet) {
		this.globalStyleSheets.add(globalStyleSheet);
	}
}
