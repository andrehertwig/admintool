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
	
	protected static final String ROOTCONTEXT = "/admintool";
	
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
		LOGGER.debug(String.format("receiving request: ctxPath: %s, uri: %s", request.getContextPath(), request.getRequestURI()));
		String name = getMenuName(request);
		
		Optional<MenuEntry> menuentry = Optional.empty();
		for (AdminComponent comp : adminTool.getComponents()) {
			menuentry = comp.getMainMenu().flattened().filter(entry -> null != entry.getName() && entry.getName().equals(name)).findFirst();
			if (menuentry.isPresent()) {
				break;
			}
		}
		if (menuentry.isPresent()) {
			model.put("contentPage", "admintool/" + menuentry.get().getTarget());
			if (null != menuentry.get().getVariables()) {
				model.putAll(menuentry.get().getVariables());
			}
			model.put("activeMenu", menuentry.get());
		} else {
			model.put("contentPage", "admintool/content/error404");
		}
		model.put("rootContext", AbstractAdminController.ROOTCONTEXT);
	}
	
	private String getMenuName(HttpServletRequest request) {
		String name = request.getRequestURI().replaceFirst(AbstractAdminController.ROOTCONTEXT, "");
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
