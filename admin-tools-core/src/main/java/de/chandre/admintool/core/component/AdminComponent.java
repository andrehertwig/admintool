package de.chandre.admintool.core.component;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * A admin component<br>
 * should have at least a minimum of one menu entry
 * @author Andre
 *
 */
public interface AdminComponent extends Comparable<AdminComponent>
{
	/**
	 * @return the displayName
	 */
	String getDisplayName();

	/**
	 * @param displayName the displayName to set
	 */
	void setDisplayName(String displayName);
	
	/**
	 * @return the mainMenu
	 */
	MenuEntry getMainMenu();
	
	/**
	 * @param mainMenu the mainMenu to set
	 */
	void setMainMenu(MenuEntry mainMenu);
	
	/**
	 * @return the notificationTemplates
	 */
	List<String> getNotificationTemplates();
	
	/**
	 * path to notification template shown in the top right menu<br>
	 * should start with a "li" tag. eg:<br>
	 * <code>
	 * 		&lt;li class="dropdown messages-menu"&gt;
	 * </code>
	 * @see https://almsaeedstudio.com/themes/AdminLTE/documentation/index.html#component-main-header
	 * 
	 * @param notificationTemplates the notificationTemplates to set
	 */
	void setNotificationTemplates(List<String> notificationTemplates);
	
	/**
	 * path to notification template shown in the top right menu<br>
	 * should start with a "li" tag. eg:<br>
	 * <code>
	 * 		&lt;li class="dropdown messages-menu"&gt;
	 * </code>
	 * @see https://almsaeedstudio.com/themes/AdminLTE/documentation/index.html#component-main-header
	 * 
	 * @param notificationTemplate the template path to notification template
	 */
	void addNotificationTemplate(String notificationTemplate);
	
	/**
	 * @return the additionalCSS
	 */
	Map<String, Boolean> getAdditionalCSS();
	
	/**
	 *  map with paths to CSS.<br>
	 * 
	 * @param additionalCSS the additionalCSS to set
	 * @see #addAdditionalCSS(String, boolean)
	 */
	void setAdditionalCSS(Map<String, Boolean> additionalCSS);
	
	/**
	 * path to CSS.<br>
	 * Example:<br>
	 *  <code>
	 *  addAdditionalCSS("/static/myComponent/css/myStyles.css", true);<br>
	 *  addAdditionalCSS("http://example.com/styles.css", false);
	 *  </code>
	 * @param additionalCSS the additionalCSS to set
	 */
	void addAdditionalCSS(String additionalCSS, boolean relative);
	
	/**
	 * @return the additionalJS
	 */
	Map<String, Boolean> getAdditionalJS();
	
	/**
	 * map with path to additional JavaScript files. <br>
	 * @param additionalJS the additionalJS to set
	 * @see #addAdditionalJS(String, boolean)
	 */
	void setAdditionalJS(Map<String, Boolean> additionalJS);
	
	/**
	 * path to additional JavaScript files.<br>
	 * Example:<br>
	 *  <code>
	 *  addAdditionalJS("/static/myComponent/js/myScripts.js", true);<br>
	 *  addAdditionalJS("http://example.com/script.js", false);
	 *  </code>
	 * @param additionalJS the additionalJS to set
	 */
	void addAdditionalJS(String additionalJS, boolean relative);
	
	/**
	 * should return a list of roles which should be able to access this component 
	 * @return
	 * @since 1.0.1
	 */
	Set<String> getSecurityRoles();

	/**
	 * security role like "ROLE_USER" or for anonymous "ROLE_ANONYMOUS" validated with spring security<br>
	 * requires: admin-tool-core-security<br>
	 * only for displaying or hiding menu entries
	 * @param securityRole
	 * @since 1.0.1
	 */
	void addSecurityRole(String securityRole);

	/**
	 * position of component in menu. Initially set to {@link Integer#MAX_VALUE}
	 * @return
	 * @since 1.0.1
	 */
	Integer getPosition();

	/**
	 * position of component in menu. Initially set to {@link Integer#MAX_VALUE}
	 * @param position
	 * @since 1.0.1
	 */
	void setPosition(Integer position);
}
