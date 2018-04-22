package de.chandre.admintool.core.component;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * A admin component<br>
 * should have at least a minimum of one menu entry
 * 
 * @author Andre
 * @since 1.0.0
 */
public class AdminComponentImpl implements AdminComponent
{
	private String displayName;
	
	private MenuEntry mainMenu;
	
	private List<String> notificationTemplates = new ArrayList<>(0);
	
	private Map<String, Boolean> additionalCSS = new LinkedHashMap<>(1);
	private Map<String, Boolean> additionalJS = new LinkedHashMap<>(1);
	
	private Set<String> securityRoles = new HashSet<>();
	
	private Integer position;

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
		mainMenu.setComponent(this);
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

	/**
	 * @since 1.0.1
	 * @return the securityRoles
	 * @see #addSecurityRole(String)
	 */
	@Override
	public Set<String> getSecurityRoles() {
		return securityRoles;
	}

	/**
	 * 
	 * @since 1.0.1
	 * @param securityRoles the securityRoles to set
	 * @see #addSecurityRole(String)
	 */
	public void setSecurityRoles(Set<String> securityRoles) {
		this.securityRoles = securityRoles;
	}
	
	@Override
	public void addSecurityRole(String securityRole) {
		this.securityRoles.add(securityRole);
	}

	/**
	 * @since 1.0.1
	 * @return the position
	 */
	@Override
	public Integer getPosition() {
		return position;
	}

	/**
	 * @since 1.0.1
	 * @param position the position to set
	 */
	@Override
	public void setPosition(Integer position) {
		this.position = position;
	}
	
	/**
	 * compares the displayName
	 */
	@Override
	public int compareTo(AdminComponent o) {
		return displayName.compareTo(o.getDisplayName());
	}
	
	

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((displayName == null) ? 0 : displayName.hashCode());
		result = prime * result + ((mainMenu == null) ? 0 : mainMenu.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AdminComponentImpl other = (AdminComponentImpl) obj;
		if (additionalCSS == null) {
			if (other.additionalCSS != null)
				return false;
		} else if (!additionalCSS.equals(other.additionalCSS))
			return false;
		if (additionalJS == null) {
			if (other.additionalJS != null)
				return false;
		} else if (!additionalJS.equals(other.additionalJS))
			return false;
		if (displayName == null) {
			if (other.displayName != null)
				return false;
		} else if (!displayName.equals(other.displayName))
			return false;
		if (mainMenu == null) {
			if (other.mainMenu != null)
				return false;
		} else if (!mainMenu.equals(other.mainMenu))
			return false;
		if (notificationTemplates == null) {
			if (other.notificationTemplates != null)
				return false;
		} else if (!notificationTemplates.equals(other.notificationTemplates))
			return false;
		if (position == null) {
			if (other.position != null)
				return false;
		} else if (!position.equals(other.position))
			return false;
		if (securityRoles == null) {
			if (other.securityRoles != null)
				return false;
		} else if (!securityRoles.equals(other.securityRoles))
			return false;
		return true;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AdminComponentImpl [displayName=").append(displayName).append(", mainMenu=").append(mainMenu)
				.append(", notificationTemplates=").append(notificationTemplates).append(", additionalCSS=")
				.append(additionalCSS).append(", additionalJS=").append(additionalJS).append(", securityRoles=")
				.append(securityRoles).append(", position=").append(position).append("]");
		return builder.toString();
	}
	
	/**
	 * admin component builder for chained calls
	 * @return
	 */
	public static AdminComponentBuilder builder() {
		return new AdminComponentBuilder();
	}
	
	/**
	 * static class with possibility for chained calls
	 * @author André
	 * @since 1.1.6
	 */
	public static class AdminComponentBuilder {
		private AdminComponent component = new AdminComponentImpl();
		
		/**
		 * @see AdminComponent#setDisplayName(String)
		 * @param displayName
		 * @return
		 */
		public AdminComponentBuilder displayName(String displayName) {
			component.setDisplayName(displayName);
			return this;
		}
		/**
		 * @see AdminComponent#setMainMenu(MenuEntry)
		 * @param mainMenu
		 * @return
		 */
		public AdminComponentBuilder mainMenu(MenuEntry mainMenu) {
			component.setMainMenu(mainMenu);
			return this;
		}
		/**
		 * @see AdminComponent#setNotificationTemplates(List)
		 * @param notificationTemplates
		 * @return
		 */
		public AdminComponentBuilder notificationTemplates(List<String> notificationTemplates) {
			component.setNotificationTemplates(notificationTemplates);
			return this;
		}
		/**
		 * @see AdminComponent#addNotificationTemplate(String)
		 * @param notificationTemplate
		 * @return
		 */
		public AdminComponentBuilder notificationTemplate(String notificationTemplate) {
			component.addNotificationTemplate(notificationTemplate);
			return this;
		}
		/**
		 * @see AdminComponent#setAdditionalCSS(Map)
		 * @param additionalCSS
		 * @return
		 */
		public AdminComponentBuilder additionalCSS(Map<String, Boolean> additionalCSS) {
			component.setAdditionalCSS(additionalCSS);
			return this;
		}
		/**
		 * @see AdminComponent#addAdditionalCSS(String, boolean)
		 * @param additionalCSS
		 * @param relative
		 * @return
		 */
		public AdminComponentBuilder additionalCSS(String additionalCSS, boolean relative) {
			component.addAdditionalCSS(additionalCSS, relative);
			return this;
		}
		/**
		 * @see AdminComponent#setAdditionalJS(Map)
		 * @param additionalJS
		 * @return
		 */
		public AdminComponentBuilder additionalJS(Map<String, Boolean> additionalJS) {
			component.setAdditionalJS(additionalJS);
			return this;
		}
		/**
		 * @see AdminComponent#addAdditionalJS(String, boolean)
		 * @param additionalJS
		 * @param relative
		 * @return
		 */
		public AdminComponentBuilder additionalJS(String additionalJS, boolean relative) {
			component.addAdditionalJS(additionalJS, relative);
			return this;
		}
		/**
		 * @see AdminComponent#addSecurityRole(String)
		 * @param securityRole
		 * @return
		 */
		public AdminComponentBuilder securityRole(String securityRole) {
			component.addSecurityRole(securityRole);
			return this;
		}
		/**
		 * @see AdminComponent#getSecurityRoles().addAll(set)
		 * @param securityRoles
		 * @return
		 */
		public AdminComponentBuilder securityRoles(Set<String> securityRoles) {
			component.getSecurityRoles().addAll(securityRoles);
			return this;
		}
		/**
		 * @see AdminComponent#setPosition(Integer)
		 * @param position
		 * @return
		 */
		public AdminComponentBuilder position(Integer position) {
			component.setPosition(position);
			return this;
		}
		/**
		 * @return the {@link AdminComponent}
		 */
		public AdminComponent build() {
			return component;
		}
	}
	
}
