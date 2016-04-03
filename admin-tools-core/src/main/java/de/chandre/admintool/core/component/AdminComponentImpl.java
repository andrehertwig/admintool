package de.chandre.admintool.core.component;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * A admin component<br>
 * should have at least a minimum of one menu entry
 * @author Andre
 *
 */
public class AdminComponentImpl implements AdminComponent
{
	private String displayName;
	
	private MenuEntry mainMenu;
	
	private List<String> notificationTemplates = new ArrayList<>(0);
	
	private Map<String, Boolean> additionalCSS = new LinkedHashMap<>(1);
	private Map<String, Boolean> additionalJS = new LinkedHashMap<>(1);

	/**
	 * @return the displayName
	 */
	@Override
	public String getDisplayName() {
		return displayName;
	}

	/**
	 * @param displayName the displayName to set
	 */
	@Override
	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	/**
	 * @return the mainMenu
	 */
	@Override
	public MenuEntry getMainMenu() {
		return mainMenu;
	}

	/**
	 * @param mainMenu the mainMenu to set
	 */
	@Override
	public void setMainMenu(MenuEntry mainMenu) {
		this.mainMenu = mainMenu;
	}

	/**
	 * @return the notificationTemplates
	 */
	@Override
	public List<String> getNotificationTemplates() {
		return notificationTemplates;
	}

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
	@Override
	public void setNotificationTemplates(List<String> notificationTemplates) {
		this.notificationTemplates = notificationTemplates;
	}

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
	@Override
	public void addNotificationTemplate(String notificationTemplate) {
		this.notificationTemplates.add(notificationTemplate);
	}

	/**
	 * @return the additionalCSS
	 */
	@Override
	public Map<String, Boolean> getAdditionalCSS() {
		return additionalCSS;
	}

	/**
	 *  map with paths to CSS.<br>
	 * 
	 * @param additionalCSS the additionalCSS to set
	 * @see #addAdditionalCSS(String, boolean)
	 */
	@Override
	public void setAdditionalCSS(Map<String, Boolean> additionalCSS) {
		this.additionalCSS = additionalCSS;
	}
	
	/**
	 * path to CSS.<br>
	 * Example:<br>
	 *  <code>
	 *  addAdditionalCSS("/static/myComponent/css/myStyles.css", true);<br>
	 *  addAdditionalCSS("http://example.com/styles.css", false);
	 *  </code>
	 * @param additionalCSS the additionalCSS to set
	 */
	@Override
	public void addAdditionalCSS(String additionalCSS, boolean relative) {
		this.additionalCSS.put(additionalCSS, relative);
	}

	/**
	 * @return the additionalJS
	 */
	@Override
	public Map<String, Boolean> getAdditionalJS() {
		return additionalJS;
	}

	/**
	 * map with path to additional JavaScript files. <br>
	 * @param additionalJS the additionalJS to set
	 * @see #addAdditionalJS(String, boolean)
	 */
	@Override
	public void setAdditionalJS(Map<String, Boolean> additionalJS) {
		this.additionalJS = additionalJS;
	}
	
	/**
	 * path to additional JavaScript files.<br>
	 * Example:<br>
	 *  <code>
	 *  addAdditionalJS("/static/myComponent/js/myScripts.js", true);<br>
	 *  addAdditionalJS("http://example.com/script.js", false);
	 *  </code>
	 * @param additionalJS the additionalJS to set
	 */
	@Override
	public void addAdditionalJS(String additionalJS, boolean relative) {
		this.additionalJS.put(additionalJS, relative);
	}
	
	@Override
	public int compareTo(AdminComponent o) {
		return displayName.compareTo(o.getDisplayName());
	}
}
