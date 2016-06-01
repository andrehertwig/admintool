package de.chandre.admintool.core.controller;

import java.util.Locale;
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
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.MenuEntry;

/**
 * general methods for admintool controllers
 * @author Andre
 *
 */
public class AbstractAdminController
{
	private static final Log LOGGER = LogFactory.getLog(AbstractAdminController.class);
	
	protected static final String ROOTCONTEXT_NAME = "admintool";
	public static final String ROOTCONTEXT = "/" + ROOTCONTEXT_NAME;
	
	@Autowired 
	private AdminTool adminTool;
	
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
		final String name = getMenuName(request, overrideName);
		LOGGER.debug("name=" +name);
		
		Optional<MenuEntry> menuentry = Optional.empty();
		for (AdminComponent comp : adminTool.getComponents()) {
			menuentry = comp.getMainMenu().flattened().filter(entry -> null != entry.getName() && entry.getName().equals(name)).findFirst();
			if (menuentry.isPresent()) {
				break;
			}
		}
		if (menuentry.isPresent()) {
			model.put("contentPage", ROOTCONTEXT_NAME + "/" + (StringUtils.isEmpty(overrideTarget) ? menuentry.get().getTarget() : overrideTarget));
			if (null != menuentry.get().getVariables()) {
				model.putAll(menuentry.get().getVariables());
			}
			model.put("activeMenu", menuentry.get());
		} else {
			model.put("contentPage", ROOTCONTEXT_NAME + "/content/error404");
		}
		model.put("rootContext", request.getContextPath() + ROOTCONTEXT);
	}
	
	private String getMenuName(HttpServletRequest request, String overrideName) {
		if (!StringUtils.isEmpty(overrideName)) {
			return overrideName;
		}
		String name = request.getRequestURI().replaceFirst(ROOTCONTEXT, "");
		if (!StringUtils.isEmpty(request.getContextPath())) {
			name = name.replaceFirst(request.getContextPath(), "");
		}
		if (name.startsWith("/")) {
			name = name.substring(1, name.length());
		}
		return name;
	}
	
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
