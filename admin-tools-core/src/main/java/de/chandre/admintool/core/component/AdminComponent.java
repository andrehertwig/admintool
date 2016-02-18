package de.chandre.admintool.core.component;

import java.util.ArrayList;
import java.util.List;

public class AdminComponent implements Comparable<AdminComponent>
{
	private String displayName;
	
	private MenuEntry mainMenu;
	
	private List<String> notificationTemplates = new ArrayList<>(0);
	
	private List<String> additionalCSS = new ArrayList<>(1);
	private List<String> additionalJS = new ArrayList<>(1);

	/**
	 * @return the displayName
	 */
	public String getDisplayName() {
		return displayName;
	}

	/**
	 * @param displayName the displayName to set
	 */
	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	/**
	 * @return the mainMenu
	 */
	public MenuEntry getMainMenu() {
		return mainMenu;
	}

	/**
	 * @param mainMenu the mainMenu to set
	 */
	public void setMainMenu(MenuEntry mainMenu) {
		this.mainMenu = mainMenu;
	}

	/**
	 * @return the notificationTemplates
	 */
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
	public void addNotificationTemplate(String notificationTemplate) {
		this.notificationTemplates.add(notificationTemplate);
	}

	/**
	 * @return the additionalCSS
	 */
	public List<String> getAdditionalCSS() {
		return additionalCSS;
	}

	/**
	 * list of relative path to CSS. no prefixes will be added.<br>
	 * e.g: /static/admintool/myApp/mycss.css
	 * 
	 * @param additionalCSS the additionalCSS to set
	 */
	public void setAdditionalCSS(List<String> additionalCSS) {
		this.additionalCSS = additionalCSS;
	}
	
	/**
	 * relative path to CSS. no prefixes will be added.<br>
	 * e.g: /static/admintool/myApp/mycss.css
	 * 
	 * @param additionalCSS the additionalCSS to set
	 */
	public void addAdditionalCSS(String additionalCSS) {
		this.additionalCSS.add(additionalCSS);
	}

	/**
	 * @return the additionalJS
	 */
	public List<String> getAdditionalJS() {
		return additionalJS;
	}

	/**
	 * list of relative path to additional JavaScript files. no prefixes will be added.<br>
	 * e.g: /static/admintool/myApp/myjs.js
	 * @param additionalJS the additionalJS to set
	 */
	public void setAdditionalJS(List<String> additionalJS) {
		this.additionalJS = additionalJS;
	}
	
	/**
	 * relative path to additional JavaScript files. no prefixes will be added.<br>
	 * e.g: /static/admintool/myApp/myjs.js
	 * @param additionalJS the additionalJS to set
	 */
	public void addAdditionalJS(String additionalJS) {
		this.additionalJS.add(additionalJS);
	}
	
	@Override
	public int compareTo(AdminComponent o) {
		return displayName.compareTo(o.getDisplayName());
	}
}
