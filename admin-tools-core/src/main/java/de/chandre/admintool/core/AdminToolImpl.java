package de.chandre.admintool.core;

import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Component;

import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.AdminComponentComparator;
import de.chandre.admintool.core.component.MenuEntry;

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
	private static final Log LOGGER = LogFactory.getLog(AdminToolImpl.class);
	
	private Set<AdminComponent> components = new TreeSet<>(new AdminComponentComparator());
	
	private Map<String, Boolean> globalJavaScripts = new LinkedHashMap<>();
	private Map<String, Boolean> globalStyleSheets = new LinkedHashMap<>();
	
	@Override
	public void setComponentComparator(Comparator<AdminComponent> comparator) {
		if (null == comparator) {
			this.components = new TreeSet<>();
		} else {
			Set<AdminComponent> newComponents = new TreeSet<>(new AdminComponentComparator());
			newComponents.addAll(this.components);
			this.components = newComponents;
		}
	}

	/**
	 * @return the components
	 */
	@Override
	public Set<AdminComponent> getComponents() {
		return components;
	}
	
	/**
	 * @param components the components to set
	 */
	@Override
	public void addComponent(AdminComponent components) {
		this.components.add(components);
	}
	

	/**
	 * @param components the components to set
	 */
	@Override
	public void addComponents(Set<AdminComponent> components) {
		this.components.addAll(components);
	}

	public void setComponents(Set<AdminComponent> components) {
		this.components = components;
	}
	
	@Override
	public Map<String, Boolean> getGlobalJavaScripts() {
		return globalJavaScripts;
	}
	
	public void setGlobalJavaScripts(Map<String, Boolean> globalJavaScripts) {
		this.globalJavaScripts = globalJavaScripts;
	}
	
	@Override
	public void addGlobalJavaScript(String globalJavaScript, boolean local) {
		this.globalJavaScripts.put(globalJavaScript, local);
	}

	@Override
	public Map<String, Boolean> getGlobalStyleSheets() {
		return globalStyleSheets;
	}
	
	public void setGlobalStyleSheet(Map<String, Boolean> globalStyleSheets) {
		this.globalStyleSheets = globalStyleSheets;
	}
	
	@Override
	public void addGlobalStyleSheet(String globalStyleSheet, boolean local) {
		this.globalStyleSheets.put(globalStyleSheet, local);
	}
	
	@Override
	public MenuEntrySearchResult searchComponent(final String menuName) {
		MenuEntrySearchResult result = null;
		LOGGER.debug("search for component for menuName: " + menuName);
		Optional<MenuEntry> menuEntry = Optional.empty();
		for (AdminComponent comp : getComponents()) {
			menuEntry = comp.getMainMenu().flattened().filter(entry -> null != entry.getName() && entry.getName().equals(menuName)).findFirst();
			if (menuEntry.isPresent()) {
				result = new MenuEntrySearchResult(comp, menuEntry.get());
				break;
			}
		}
		if (null == result && menuName.lastIndexOf('/') != -1) {
			result = searchComponent(menuName.substring(0, menuName.lastIndexOf('/')));
		}
		return result;
	}
}
