package de.chandre.admintool.core.component;

import java.io.Serializable;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

/**
 * Menu entry for left main menu<br>
 * <i>displayName</i> is required on every item, <i>name</i> and <i>target</i> only if entry has no <i>submenu</i>
 * 
 * @author Andre
 *
 */
public class MenuEntry implements Serializable
{
	private static final long serialVersionUID = 1L;
	
	private String name;
	private String displayName;
	private String resouceMessageKey;
	private String target;
	private boolean hide;
	
	private MenuEntry parent;
	private List<MenuEntry> submenu = new LinkedList<>();
	
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
	 * <b>not implemented</b>
	 * @return the resouceMessageKey
	 */
	public String getResouceMessageKey() {
		return resouceMessageKey;
	}

	/**
	 *  <b>not implemented</b>
	 * @param resouceMessageKey the resouceMessageKey to set
	 */
	public void setResouceMessageKey(String resouceMessageKey) {
		this.resouceMessageKey = resouceMessageKey;
	}

	/**
	 * relative path to target template
	 * @return the target
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
	 * @param target the target to set
	 */
	public void setTarget(String target) {
		this.target = target;
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
	 * @return the variables
	 */
	public Map<String, Object> getVariables() {
		return variables;
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
	
}
