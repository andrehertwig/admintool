package de.chandre.admintool.core;

import java.util.Comparator;
import java.util.Map;
import java.util.Set;

import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.ui.ATFooterInformation;

/**
 * AdminTool core interface to get and add components.<br>
 * a component represents a main menu entry with possible sub menu entries<br>
 * it's also possible to add custom java scripts and css
 * 
 * @author Andre
 *
 */
public interface AdminTool {
	
	/**
	 * just a slash
	 */
	String SLASH = "/";
	/**
	 * name of admintool.
	 */
	String ROOTCONTEXT_NAME = "admintool";
	
	/**
	 * root context path with a leading slash
	 */
	String ROOTCONTEXT = SLASH + ROOTCONTEXT_NAME;
	
	/**
	 * path to default error template: admintool/content/error
	 */
	String GENERIC_ERROR_TPL_PATH = ROOTCONTEXT_NAME + SLASH + "content" + SLASH + "error";
	
	/**
	 * template name of "deactivated" template
	 */
	String GENERIC_DEACTIVATED_TEMPLATE_NAME = "deactivated";
	/**
	 * template path to default deactivated template
	 */
	String GENERIC_DEACTIVATED_TEMPLATE_TPL_PATH = ROOTCONTEXT_NAME + SLASH + GENERIC_DEACTIVATED_TEMPLATE_NAME;
	
	/**
	 * prefix for for resource messages
	 * @since 1.1.6
	 */
	String RESOURCE_MESSAGE_KEY_PREFIX = "ui." + ROOTCONTEXT_NAME + ".core.";
	
	/**
	 * to set a custom comparator for ordering components.
	 * 
	 * @since 1.0.1
	 * @param comparator own comparator or null. if null the default compare implementation of AdminComponent will be used
	 */
	void setComponentComparator(Comparator<AdminComponent> comparator);
	
	/**
	 * 
	 * @return
	 */
	Set<AdminComponent> getComponents();
	
	/**
	 * 
	 * @param components
	 */
	void addComponent(AdminComponent components);
	
	/**
	 * 
	 * @param components
	 */
	void addComponents(Set<AdminComponent> components);
	
	/**
	 *  A map with script url as key and a boolean if it's a relative url<br>
	 *  Example:<br>
	 *  <code>
	 *  getGlobalJavaScripts().put("/static/myComponent/js/myScript.js", true);<br>
	 *  getGlobalJavaScripts().put("http://example.com/script.js", false);
	 *  </code>
	 * @return
	 */
	Map<String, Boolean> getGlobalJavaScripts();
	
	/**
	 * Example:<br>
	 *  <code>
	 *  addGlobalJavaScript("/static/myComponent/js/myScript.js", true);<br>
	 *  addGlobalJavaScript("http://example.com/script.js", false);
	 *  </code>
	 *  
	 * @param globalJavaScript
	 * @param relative if url is relative (not absolute) and pointing to the server
	 */
	void addGlobalJavaScript(String globalJavaScript, boolean relative);
	
	/**
	 * A map with css url as key and a boolean if it's a relative url 
	 * Example:<br>
	 *  <code>
	 *  getGlobalStyleSheets().put("/static/myComponent/css/myStyles.css", true);<br>
	 *  getGlobalStyleSheets().put("http://example.com/styles.css", false);
	 *  </code>
	 * @return
	 */
	Map<String, Boolean> getGlobalStyleSheets();
	
	/**
	 * Example:<br>
	 *  <code>
	 *  addGlobalStyleSheet("/static/myComponent/css/myStyles.css", true);<br>
	 *  addGlobalStyleSheet("http://example.com/styles.css", false);
	 *  </code>
	 *  
	 * @param globalStyleSheet
	 * @param relative if url is relative (not absolute) and pointing to the server
	 */
	void addGlobalStyleSheet(String globalStyleSheet, boolean relative);

	/**
	 * searches for a menuEntry with specified name
	 * @since 1.0.1
	 * @param menuName
	 * @return null or MenuEntrySearchResult
	 */
	MenuEntrySearchResult searchComponent(String menuName);

	/**
	 * Returns the version of admintool
	 * @since 1.1.6.3
	 * @return admintool version
	 */
	String getVersion();

	/**
	 * @since 1.2.0
	 * @return null or footer information class
	 */
	ATFooterInformation getFooterInformation();

	/**
	 * @since 1.2.0
	 * @param footerInformation footer information class
	 */
	void setFooterInformation(ATFooterInformation footerInformation);
}
