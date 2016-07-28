package de.chandre.admintool.core.controller;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.propertyeditors.LocaleEditor;
import org.springframework.ui.ModelMap;
import org.springframework.util.StringUtils;
import org.springframework.web.servlet.LocaleResolver;
import org.springframework.web.servlet.support.RequestContextUtils;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.MenuEntrySearchResult;
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.MenuEntry;
import de.chandre.admintool.core.utils.AdminToolMenuUtils;

/**
 * general methods for admintool controllers
 * @author Andre
 *
 */
public class AbstractAdminController
{
	private static final Log LOGGER = LogFactory.getLog(AbstractAdminController.class);
	
	private static final String COMPONENT = "component";
	private static final String MENUITEM = "item";
	
	@Autowired 
	private AdminTool adminTool;

	@Autowired 
	private AdminToolMenuUtils menuUtils;
	
	/**
	 * sets common context variables<br>
	 * <ul>
	 * 	<li>contentPage</li>
	 * 	<li>activeMenuName</li>
	 * 	<li>activeMenu</li>
	 * 	<li>rootContext</li>
	 * 	<li></li>
	 * </ul>
	 * 
	 * @param model
	 * @param request
	 */
	protected void addCommonContextVars(ModelMap model, HttpServletRequest request) 
	{
		addCommonContextVars(model, request, null, null);
	}
	
	/**
	 * sets common context variables and will override the template name.<br>
	 * this method can be used if you want your special request mappings should resolve a other template 
	 * <ul>
	 * 	<li>contentPage</li>
	 * 	<li>activeMenuName</li>
	 * 	<li>activeMenu</li>
	 * 	<li>rootContext</li>
	 * 	<li></li>
	 * </ul>
	 * 
	 * 
	 * @param model
	 * @param request
	 * @param overrideName
	 */
	protected void addCommonContextVars(ModelMap model, HttpServletRequest request, String overrideName, String overrideTarget) 
	{
		LOGGER.debug(String.format("receiving request: ctxPath: %s, uri: %s", request.getContextPath(), request.getRequestURI()));
		final String name = menuUtils.getMenuName(request, overrideName);
		
		MenuEntrySearchResult result = adminTool.searchComponent(name);
		model.put("rootContext", request.getContextPath() + AdminTool.ROOTCONTEXT);
		
		if (null != result) {
			LOGGER.trace("Component found: " + String.valueOf(null != result.getComponent()) +
					" | menu found: " + String.valueOf(result.getMenuEntry()));
			
			model.put(MenuEntrySearchResult.NAME, result);
			
			MenuEntry entry = result.getMenuEntry();
//			//check security
//			if (!isAllowed(model, result.getComponent(), entry)) {
//				return;
//			}
			//set alternative target
			String targetPage = (StringUtils.isEmpty(overrideTarget) ? entry.getTarget() : overrideTarget);
			model.put("contentPage", AdminTool.ROOTCONTEXT_NAME + "/" + targetPage);
			if (null != entry.getVariables()) {
				model.putAll(entry.getVariables());
			}
			model.put("activeMenu", entry);
		} else {
			model.put("contentPage", AdminTool.ROOTCONTEXT_NAME + "/content/error404");
		}
	}
	
//	
//	protected boolean checkForPath(ModelMap model, HttpServletRequest request, String overrideName, HttpServletResponse response) {
//		LOGGER.debug(String.format("checking request accessibility: ctxPath: %s, uri: %s", request.getContextPath(), request.getRequestURI()));
//		final String name = menuUtils.getMenuName(request, overrideName);
//		
//		Map<String, Object> result = new HashMap<>();
//		searchComponent(name, result);
//		AdminComponent component = (AdminComponent) result.get(COMPONENT);
//		Optional<MenuEntry> menuentry = (Optional<MenuEntry>) result.get(MENUITEM);
//		LOGGER.trace("Component found: " + String.valueOf(null != component) + " | menu found: " + String.valueOf(menuentry.isPresent()));
//		
//		if (menuentry.isPresent()) {
//			MenuEntry entry = menuentry.get();
//			//check security
//			if (!isAllowed(model, component, entry)) {
//				response.setStatus(401);
//				return false;
//			}
//			return true;
//		}
//		return false;
//	}
//	
//	private void searchComponent(final String name, Map<String, Object> result) {
//		LOGGER.debug("search for component=" + name);
//		Optional<MenuEntry> menuentry = Optional.empty();
//		for (AdminComponent comp : adminTool.getComponents()) {
//			menuentry = comp.getMainMenu().flattened().filter(entry -> null != entry.getName() && entry.getName().equals(name)).findFirst();
//			result.put(MENUITEM, menuentry);
//			if (menuentry.isPresent()) {
//				result.put(COMPONENT, comp);
//				break;
//			}
//		}
//		if (null == result.get(COMPONENT) && name.lastIndexOf('/') != -1) {
//			searchComponent(name.substring(0, name.lastIndexOf('/')), result);
//		}
//	}
	
//	/**
//	 * checks if access to component and entry are allowed
//	 * @param model (optional)
//	 * @param component
//	 * @param entry (optional)
//	 * @return
//	 */
//	protected boolean isAllowed(ModelMap model, AdminComponent component, MenuEntry entry) {
//		boolean allowed = true;
//		if (!menuUtils.isPermitted(component)) {
//			allowed = false;
//		}
//		if (null != entry && !menuUtils.isPermitted(entry)) {
//			allowed = false;
//		}
//		if (!allowed && null != model) {
//			model.put("httpStatus", 401);
//			model.put("httpStatusMessage", "You are not authorized to request this page");
//			model.put("contentPage", AdminTool.ROOTCONTEXT_NAME + "/content/error");
//		}
//		return allowed;
//	}
	
	
	
	/**
	 * manually resolve of locale ... to request. useful for path variables
	 * 
	 * @param language locale representation as string
	 * @param request
	 * @param response
	 */
	protected void resolveLocale(String language, HttpServletRequest request, HttpServletResponse response){
		final LocaleEditor localeEditor = new LocaleEditor();
        localeEditor.setAsText(language);
        resolveLocale((Locale) localeEditor.getValue(), request, response);
	}

	/**
	 * manually resolve of locale ... to request. useful for path variables
	 * 
	 * @param language
	 * @param request
	 * @param response
	 */
	protected void resolveLocale(Locale language, HttpServletRequest request, HttpServletResponse response){
		final LocaleResolver localeResolver = RequestContextUtils.getLocaleResolver(request);
		localeResolver.setLocale(request, response, language);
	}
}
