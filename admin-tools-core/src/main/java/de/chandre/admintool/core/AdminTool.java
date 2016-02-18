package de.chandre.admintool.core;

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
@Component
public class AdminTool
{
	private Set<AdminComponent> components = new TreeSet<>();

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
	
}
