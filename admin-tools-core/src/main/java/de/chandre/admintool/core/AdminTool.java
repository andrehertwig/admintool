package de.chandre.admintool.core;

import java.util.Map;
import java.util.Set;

import de.chandre.admintool.core.component.AdminComponent;

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
	 * name of admintool.
	 */
	String ROOTCONTEXT_NAME = "admintool";
	
	/**
	 * root context path with a leading slash
	 */
	String ROOTCONTEXT = "/" + ROOTCONTEXT_NAME;
	
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
	 *  A map with script url as key and a boolean if it's a relative url<br>
	 *  Example:<br>
	 *  <code>
	 *  getGlobalJavaScripts().put("/static/myComponent/js/myScript.js", true);<br>
	 *  getGlobalJavaScripts().put("http://example.com/script.js", false);
	 *  </code>
	 * @return
	 */
	public Map<String, Boolean> getGlobalJavaScripts();
	
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
	public void addGlobalJavaScript(String globalJavaScript, boolean relative);
	
	/**
	 * A map with css url as key and a boolean if it's a relative url 
	 * Example:<br>
	 *  <code>
	 *  getGlobalStyleSheets().put("/static/myComponent/css/myStyles.css", true);<br>
	 *  getGlobalStyleSheets().put("http://example.com/styles.css", false);
	 *  </code>
	 * @return
	 */
	public Map<String, Boolean> getGlobalStyleSheets();
	
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
	public void addGlobalStyleSheet(String globalStyleSheet, boolean relative);

	/**
	 * searches for a menuEntry with specified name
	 * @since 1.0.1
	 * @param menuName
	 * @return null or MenuEntrySearchResult
	 */
	MenuEntrySearchResult searchComponent(String menuName);
}
