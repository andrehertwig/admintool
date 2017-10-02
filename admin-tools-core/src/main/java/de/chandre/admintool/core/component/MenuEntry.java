package de.chandre.admintool.core.component;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import org.springframework.util.CollectionUtils;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.utils.AdminToolMenuUtils;

/**
 * Menu entry for left main menu<br>
 * <i>displayName</i> is required on every item, <i>name</i> and <i>target</i> only if entry has no <i>submenu</i>
 * 
 * @author Andre
 * @since 1.0.0
 */
public class MenuEntry implements Serializable
{
	private static final long serialVersionUID = 1L;
	
	private String name;
	private String displayName;
	private String resouceMessageKey;
	private String target;
	private boolean hide = false;
	private String activeName;
	
	private AdminComponent component;
	private MenuEntry parent;
	private List<MenuEntry> submenu = new LinkedList<>();
	
	private boolean useCCSHierarchy = false;
	private Map<String, Boolean> additionalCSS = new LinkedHashMap<>(1);
	private boolean useJSHierarchy = false;
	private Map<String, Boolean> additionalJS = new LinkedHashMap<>(1);
	
	private Set<String> securityRoles = new HashSet<>();
	
	private Map<String, Object> variables = new HashMap<>();
	
	public MenuEntry() {
		super();
		this.hide = false;
	}
	
	/**
	 * 
	 * @param name - the link mapping 
	 * @param displayName - the display name
	 * @param target -  the template path. see {@link #setTarget(String)}
	 */
	public MenuEntry(String name, String displayName, String target) {
		super();
		this.name = name;
		this.displayName = displayName;
		this.target = target;
		this.hide = false;
	}
	
	/**
	 * @since 1.0.1
	 * @param name - the link mapping 
	 * @param displayName - the display name
	 * @param target -  the template path. see {@link #setTarget(String)}
	 * @param securityRoles - Set of roles to check against the current user for displaying/hiding menu entries in the frontend
	 */
	public MenuEntry(String name, String displayName, String target, Set<String> securityRoles) {
		super();
		this.name = name;
		this.displayName = displayName;
		this.target = target;
		this.hide = false;
		this.securityRoles = securityRoles;
	}
	
	void setComponent(AdminComponent component) {
		this.component = component;
	}
	
	AdminComponent getComponent() {
		return this.component;
	}

	/**
	 * the link mapping 
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * the link mapping 
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * display name shown in the menu
	 * @return the displayName
	 */
	public String getDisplayName() {
		return displayName;
	}

	/**
	 * display name shown in the menu
	 * @param displayName the displayName to set
	 */
	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	/**
	 * key for message resources. must be enabled in core config.
	 * @return the resouceMessageKey
	 */
	public String getResouceMessageKey() {
		return resouceMessageKey;
	}

	/**
	 * key for message resources. must be enabled in core config.
	 * @param resouceMessageKey the resouceMessageKey to set
	 */
	public void setResouceMessageKey(String resouceMessageKey) {
		this.resouceMessageKey = resouceMessageKey;
	}

	/**
	 * relative path to target template
	 * @return the target template path
	 * @see #setTarget(String)
	 */
	public String getTarget() {
		return target;
	}

	/**
	 * relative path to target template using Thymeleaf default resolve mechanism.<br>
	 * path should not start with a leading / and should not make use o any file ending<br>
	 * templates have to be in a "admintool" folder. this is used as prefix!<br>
	 * <br>
	 * <b>for example:</b><br> 
	 * your Thymeleaf is configured to look for templates in: <i>classpath:/templates</i><br>
	 * than your template has to be in: <i>/templates/admintool/</i><br>
	 * you want to use your own structure: <i>/templates/admintool/myComponent/myMenuTemplate.html</i><br>
	 * <br>
	 * to get the example resolved set target to: <i>myComponent/myMenuTemplate</i>
	 * 
	 * 
	 * @param target the target template path
	 */
	public void setTarget(String targetTemplatePath) {
		this.target = AdminToolMenuUtils.normalizeTarget(targetTemplatePath);
	}

	/**
	 * if set to true this menu entry will be hidden, but still requestable
	 * @return the hide
	 */
	public boolean isHide() {
		return hide;
	}

	/**
	 * if set to true this menu entry will be hidden, but still requestable
	 * @param hide the hide to set
	 */
	public void setHide(boolean hide) {
		this.hide = hide;
	}
	
	/**
	 * name of menu which should be marked as active (useful for hidden menuEntries)
	 * @return
	 * @since 1.1.6
	 */
	public String getActiveName() {
		return activeName;
	}

	/**
	 * name of menu which should be marked as active (useful for hidden menuEntries)
	 * @param activeName
	 * @since 1.1.6
	 */
	public void setActiveName(String activeName) {
		this.activeName = activeName;
	}

	/**
	 * the parent entry or null if root
	 * @return the parent
	 */
	public MenuEntry getParent() {
		return parent;
	}

	/**
	 * the parent entry or null if root
	 * @param parent the parent to set
	 */
	public void setParent(MenuEntry parent) {
		this.parent = parent;
	}

	/**
	 * list of sub menu entries.
	 * @return the submenu
	 */
	public List<MenuEntry> getSubmenu() {
		return submenu;
	}

	/**
	 * list of sub menu entries.
	 * @param submenu the submenu to set
	 */
	public void setSubmenu(List<MenuEntry> submenu) {
		submenu.stream().forEach(entry -> entry.setParent(this));
		this.submenu = submenu;
	}
	
	/**
	 * add a sub menu entry
	 * @param mainmenu the mainmenu to set
	 */
	public void addSubmenuEntry(MenuEntry submenu) {
		submenu.setParent(this);
		this.submenu.add(submenu);
	}

	/**
	 * 
	 * @return a stream of menu entry with all recursive resolved sub menu entries
	 */
	public Stream<MenuEntry> flattened() {
        return Stream.concat(
                Stream.of(this),
                submenu.stream().flatMap(MenuEntry::flattened));
    }
	
	/**
	 * 
	 * @return stream of parents (recursive)
	 */
	public Stream<MenuEntry> reverseFlattened() {
		if (null == parent) {
			return Stream.of(this);
		}
		return Stream.concat(
                Stream.of(this),
                Stream.of(parent).flatMap(MenuEntry::reverseFlattened));
	}
	
	/**
	 * @return the additionalCSS
	 * @since 1.0.3
	 */
	public Map<String, Boolean> getAdditionalCSS() {
		return additionalCSS;
	}

	/**
	 *  map with paths to CSS.<br>
	 * 
	 * @param additionalCSS the additionalCSS to set
	 * @see #addAdditionalCSS(String, boolean)
	 * @since 1.0.3
	 */
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
	 * @since 1.0.3
	 */
	public void addAdditionalCSS(String additionalCSS, boolean relative) {
		this.additionalCSS.put(additionalCSS, relative);
	}
	
	/**
	 * returns all additional js from this to upper menu item hierarchy beginning with the root.
	 * 
	 * @return
	 * @since 1.1.4
	 */
	public Map<String, Boolean> getAdditionalCSSReverse() {
		Map<String, Boolean> result = new LinkedHashMap<>();
		List<MenuEntry> parents = reverseFlattened().collect(AdminToolMenuUtils.toListReversed());
		parents.forEach(menuEntry -> {
			if (null != menuEntry.getAdditionalCSS()) {
				result.putAll(menuEntry.getAdditionalCSS());
			}
		});
		return result;
	}

	/**
	 * 
	 * @return
	 * @since 1.1.4
	 */
	public boolean isUseCCSHierarchy() {
		return useCCSHierarchy;
	}

	/**
	 * if this is set to true all css links up to root are collected and used.<br>
	 * this could be usefull if you want to structure your resources in a hierarchy
	 *  
	 * @param useCCSHierarchy
	 * @since 1.1.4
	 */
	public void setUseCCSHierarchy(boolean useCCSHierarchy) {
		this.useCCSHierarchy = useCCSHierarchy;
	}

	/**
	 * @return the additionalJS
	 * @since 1.0.3
	 */
	public Map<String, Boolean> getAdditionalJS() {
		return additionalJS;
	}

	/**
	 * map with path to additional JavaScript files. <br>
	 * @param additionalJS the additionalJS to set
	 * @see #addAdditionalJS(String, boolean)
	 * @since 1.0.3
	 */
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
	 * @since 1.0.3
	 */
	public void addAdditionalJS(String additionalJS, boolean relative) {
		this.additionalJS.put(additionalJS, relative);
	}
	
	/**
	 * returns all additional js from this to upper menu item hierarchy beginning with the root.
	 * 
	 * @return
	 * @since 1.1.4
	 */
	public Map<String, Boolean> getAdditionalJSReverse() {
		Map<String, Boolean> result = new LinkedHashMap<>();
		List<MenuEntry> parents = reverseFlattened().collect(AdminToolMenuUtils.toListReversed());
		parents.forEach(menuEntry -> {
			if (null != menuEntry.getAdditionalJS()) {
				result.putAll(menuEntry.getAdditionalJS());
			}
		});
		return result;
	}
	
	/**
	 * 
	 * @return
	 * @since 1.1.4
	 */
	public boolean isUseJSHierarchy() {
		return useJSHierarchy;
	}

	/**
	 * if this is set to true all css links up to root are collected and used.<br>
	 * this could be usefull if you want to structure your resources in a hierarchy
	 *  
	 * @param useCCSHierarchy
	 * @since 1.1.4
	 */
	public void setUseJSHierarchy(boolean useJSHierarchy) {
		this.useJSHierarchy = useJSHierarchy;
	}

	/**
	 * the effective roles reverse recursive to component if not set at this object or anywhere within the tree<br>
	 * first menu entry or component with roles will return it.
	 * @return a unmodifiable Set or empty Set
	 * @since 1.0.1
	 */
	public Set<String> getAffectedSecurityRoles() {
		if (CollectionUtils.isEmpty(securityRoles)) {
			Stream<MenuEntry> parents = reverseFlattened();
			Iterator<MenuEntry> entry = parents.iterator();
			while (entry.hasNext()) {
				MenuEntry menuEntry = (MenuEntry) entry.next();
				if (!CollectionUtils.isEmpty(menuEntry.getSecurityRoles())) {
					return Collections.unmodifiableSet(menuEntry.getSecurityRoles());
				}
				//root?
				if (null != menuEntry.getComponent()) {
					if (null != menuEntry.getComponent().getSecurityRoles()) {
						return Collections.unmodifiableSet(menuEntry.getComponent().getSecurityRoles());
					}
				}
			}
			return Collections.emptySet();
		}
		return Collections.unmodifiableSet(securityRoles);
	}
	
	/**
	 * @return the securityRoles
	 * @since 1.0.1
	 */
	public Set<String> getSecurityRoles() {
		return securityRoles;
	}

	/**
	 * @param securityRoles the securityRoles to set
	 * @since 1.0.1
	 */
	public void setSecurityRoles(Set<String> securityRoles) {
		this.securityRoles = securityRoles;
	}
	
	/**
	 * @param securityRole the securityRoles to set
	 * @since 1.0.1
	 */
	public void addSecurityRole(String securityRole) {
		this.securityRoles.add(securityRole);
	}

	/**
	 * returns the variables map
	 * 
	 * @return the variables
	 */
	public Map<String, Object> getVariables() {
		return variables;
	}
	
	/**
	 * returns the menu variable by key
	 * 
	 * @param key
	 * @return
	 * @since 1.1.2
	 */
	public Object getVariable(String key) {
		return variables.get(key);
	}

	/**
	 * additional variables for the template context
	 * e.g.: if you want to access a self created object within the template which is not accessible via Thymeleaf bean resolution.
	 * @param variables the variables to set
	 */
	public void setVariables(Map<String, Object> variables) {
		this.variables = variables;
	}
	
	/**
	 * add a additional variable for the template context<br>
	 * e.g.: if you want to access a self created object within the template which is not accessible via Thymeleaf bean resolution.
	 * @param variables the variables to set
	 */
	public void addVariable(String key, Object variable) {
		this.variables.put(key, variable);
	}
	
	

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
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
		MenuEntry other = (MenuEntry) obj;
		if (activeName == null) {
			if (other.activeName != null)
				return false;
		} else if (!activeName.equals(other.activeName))
			return false;
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
		if (hide != other.hide)
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (parent == null) {
			if (other.parent != null)
				return false;
		} else if (!parent.equals(other.parent))
			return false;
		if (resouceMessageKey == null) {
			if (other.resouceMessageKey != null)
				return false;
		} else if (!resouceMessageKey.equals(other.resouceMessageKey))
			return false;
		if (securityRoles == null) {
			if (other.securityRoles != null)
				return false;
		} else if (!securityRoles.equals(other.securityRoles))
			return false;
		if (submenu == null) {
			if (other.submenu != null)
				return false;
		} else if (!submenu.equals(other.submenu))
			return false;
		if (target == null) {
			if (other.target != null)
				return false;
		} else if (!target.equals(other.target))
			return false;
		if (useCCSHierarchy != other.useCCSHierarchy)
			return false;
		if (useJSHierarchy != other.useJSHierarchy)
			return false;
		if (variables == null) {
			if (other.variables != null)
				return false;
		} else if (!variables.equals(other.variables))
			return false;
		return true;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("MenuEntry [name=").append(name).append(", displayName=").append(displayName)
				.append(", resouceMessageKey=").append(resouceMessageKey).append(", target=").append(target)
				.append(", hide=").append(hide).append(", submenu=").append(submenu).append(", additionalCSS=").append(additionalCSS)
				.append(", additionalJS=").append(additionalJS).append(", securityRoles=").append(securityRoles)
				.append(", variables=").append(variables).append("]");
		return builder.toString();
	}
	
	/**
	 * menu entry builder for chained calls
	 * 
	 * @since 1.1.6
	 * @return
	 */
	public static MenuEntryBuilder builder() {
	    return new MenuEntryBuilder();
	}
	
	/**
	 * static class with possibility for chained calls
	 * 
	 * @author André
	 * @since 1.1.6
	 */
	public static class MenuEntryBuilder {
		private MenuEntry entry = new MenuEntry();
		
		/**
		 * @see MenuEntry#setName(String)
		 * @param name
		 * @return
		 */
		public MenuEntryBuilder name(String name) {
			entry.setName(name);
			return this;
		}
		/**
		 * @see MenuEntry#setDisplayName(String)
		 * @param displayName
		 * @return
		 */
		public MenuEntryBuilder displayName(String displayName) {
			entry.setDisplayName(displayName);
			return this;
		}
		/**
		 * @see MenuEntry#setResouceMessageKey(String)
		 * @param resouceMessageKey
		 * @return
		 */
		public MenuEntryBuilder resouceMessageKey(String resouceMessageKey) {
			entry.setResouceMessageKey(resouceMessageKey);
			return this;
		}
		/**
		 * used {@link AdminTool#RESOURCE_MESSAGE_KEY_PREFIX} as prefix
		 * @see MenuEntry#setResouceMessageKey(String)
		 * @param resouceMessageKey
		 * @return
		 */
		public MenuEntryBuilder resouceMessageKeySuffix(String resouceMessageKey) {
			entry.setResouceMessageKey(AdminTool.RESOURCE_MESSAGE_KEY_PREFIX + resouceMessageKey);
			return this;
		}
		/**
		 * @see MenuEntry#setTarget(String)
		 * @param targetTemplatePath
		 * @return
		 */
		public MenuEntryBuilder target(String targetTemplatePath) {
			entry.setTarget(targetTemplatePath);
			return this;
		}
		/**
		 * @see MenuEntry#setHide(boolean)
		 * @param hide
		 * @return
		 */
		public MenuEntryBuilder hide(boolean hide) {
			entry.setHide(hide);
			return this;
		}
		/**
		 * @see MenuEntry#setActiveName(String)
		 * @param activeName
		 * @return
		 */
		public MenuEntryBuilder activeName(String activeName) {
			entry.setActiveName(activeName);
			return this;
		}
		/**
		 * @see MenuEntry#setSubmenu(List)
		 * @param submenu
		 * @return
		 */
		public MenuEntryBuilder submenu(List<MenuEntry> submenu) {
			entry.setSubmenu(submenu);
			return this;
		}
		/**
		 * @see MenuEntry#addSubmenuEntry(MenuEntry)
		 * @param submenu
		 * @return
		 */
		public MenuEntryBuilder submenuEntry(MenuEntry submenu) {
			entry.addSubmenuEntry(submenu);
			return this;
		}
		/**
		 * @see MenuEntry#setAdditionalCSS(Map)
		 * @param additionalCSS
		 * @return
		 */
		public MenuEntryBuilder additionalCSS(Map<String, Boolean> additionalCSS) {
			entry.setAdditionalCSS(additionalCSS);
			return this;
		}
		/**
		 * @see MenuEntry#addAdditionalCSS(String, boolean)
		 * @param additionalCSS
		 * @param relative
		 * @return
		 */
		public MenuEntryBuilder additionalCSS(String additionalCSS, boolean relative) {
			entry.addAdditionalCSS(additionalCSS, relative);
			return this;
		}
		/**
		 * @see MenuEntry#setUseCCSHierarchy(boolean)
		 * @param useCCSHierarchy
		 * @return
		 */
		public MenuEntryBuilder useCCSHierarchy(boolean useCCSHierarchy) {
			entry.setUseCCSHierarchy(useCCSHierarchy);
			return this;
		}
		/**
		 * @see MenuEntry#setAdditionalJS(Map)
		 * @param additionalJS
		 * @return
		 */
		public MenuEntryBuilder additionalJS(Map<String, Boolean> additionalJS) {
			entry.setAdditionalJS(additionalJS);
			return this;
		}
		/**
		 * @see MenuEntry#addAdditionalJS(String, boolean)
		 * @param additionalJS
		 * @param relative
		 * @return
		 */
		public MenuEntryBuilder additionalJS(String additionalJS, boolean relative) {
			entry.addAdditionalJS(additionalJS, relative);
			return this;
		}
		/**
		 * @see MenuEntry#setUseJSHierarchy(boolean)
		 * @param useJSHierarchy
		 * @return
		 */
		public MenuEntryBuilder useJSHierarchy(boolean useJSHierarchy) {
			entry.setUseJSHierarchy(useJSHierarchy);
			return this;
		}
		/**
		 * @see MenuEntry#setSecurityRoles(Set)
		 * @param securityRoles
		 * @return
		 */
		public MenuEntryBuilder securityRoles(Set<String> securityRoles) {
			entry.setSecurityRoles(securityRoles);
			return this;
		}
		/**
		 * @see MenuEntry#addSecurityRole(String)
		 * @param securityRole
		 * @return
		 */
		public MenuEntryBuilder securityRole(String securityRole) {
			entry.addSecurityRole(securityRole);
			return this;
		}
		/**
		 * @see MenuEntry#setVariables(Map)
		 * @param variables
		 * @return
		 */
		public MenuEntryBuilder variables(Map<String, Object> variables) {
			entry.setVariables(variables);
			return this;
		}
		/**
		 * @see MenuEntry#addVariable(String, Object)
		 * @param key
		 * @param variable
		 * @return
		 */
		public MenuEntryBuilder variable(String key, Object variable) {
			entry.addVariable(key, variable);
			return this;
		}
		/**
		 * @return the menu entry
		 */
		public MenuEntry build() {
			return entry;
		}
	}
}
