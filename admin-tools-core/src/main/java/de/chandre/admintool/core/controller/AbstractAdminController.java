package de.chandre.admintool.core.controller;

import java.util.Locale;
import java.util.regex.Pattern;

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
import de.chandre.admintool.core.AdminToolCoreConfig;
import de.chandre.admintool.core.MenuEntrySearchResult;
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
	
	@Autowired 
	private AdminTool adminTool;

	@Autowired 
	private AdminToolMenuUtils menuUtils;
	
	@Autowired
	private AdminToolCoreConfig coreConfig;
	
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
	 * @return 
	 */
	protected String addCommonContextVars(ModelMap model, HttpServletRequest request) 
	{
		return addCommonContextVars(model, request, null, null);
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
	protected String addCommonContextVars(ModelMap model, HttpServletRequest request, String overrideName, String overrideTarget) 
	{
		LOGGER.debug(String.format("receiving request: ctxPath: %s, uri: %s", request.getContextPath(), request.getRequestURI()));
		final String name = menuUtils.getMenuName(request, overrideName);
		
		//get menu entry by name
		MenuEntrySearchResult result = adminTool.searchComponent(name);
		model.put("rootContext", getRootContext(request));
		model.put("adminToolContext", AdminTool.ROOTCONTEXT);
		String targetTpl = "/content/error404";
		if (null != result) {
			LOGGER.trace("Component found: " + String.valueOf(null != result.getComponent()) +
					" | menu found: " + String.valueOf(result.getMenuEntry()));
			
			model.put(MenuEntrySearchResult.NAME, result);
			MenuEntry entry = result.getMenuEntry();
			//set alternative target
			targetTpl = (StringUtils.isEmpty(overrideTarget) ? entry.getTarget() : overrideTarget);
			model.put("contentPage", AdminTool.ROOTCONTEXT_NAME + AdminTool.SLASH + targetTpl);
			if (null != entry.getVariables()) {
				model.putAll(entry.getVariables());
			}
			model.put("activeMenu", entry);
		} else {
			model.put("contentPage", AdminTool.ROOTCONTEXT_NAME + AdminTool.SLASH + targetTpl);
		}
		return targetTpl;
	}
	
	protected String getRootContext(HttpServletRequest request) {
		if (StringUtils.isEmpty(coreConfig.getStripRootContext())) {
			return request.getContextPath() + AdminTool.ROOTCONTEXT;
		}
		return request.getContextPath().replaceFirst(Pattern.quote(coreConfig.getStripRootContext()), "") + AdminTool.ROOTCONTEXT;
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
