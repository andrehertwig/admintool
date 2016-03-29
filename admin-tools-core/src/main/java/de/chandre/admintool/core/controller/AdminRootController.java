package de.chandre.admintool.core.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;

import net.bull.javamelody.MonitoredWithSpring;

/**
 * the root admin controller serving templates generally
 * @author Andre
 *
 */
@Controller
@RequestMapping(AbstractAdminController.ROOTCONTEXT)
@MonitoredWithSpring
public class AdminRootController extends AbstractAdminController
{
	private static final Log LOGGER = LogFactory.getLog(AdminRootController.class);
	
	@RequestMapping(value = {"", "/",})
	public String startPage(ModelMap model, HttpServletRequest request) {
		
		if(LOGGER.isTraceEnabled()) LOGGER.trace("serving admin root page");
		model.put("rootContext", request.getContextPath() + AbstractAdminController.ROOTCONTEXT);
		model.put("contentPage", "admintool/content/start");
		return AbstractAdminController.ROOTCONTEXT_NAME + "/index";
	}
	
	@RequestMapping(value = {"/**",})
	public String subPage(ModelMap model, HttpServletRequest request) {
		
		addCommonContextVars(model, request);
		return AbstractAdminController.ROOTCONTEXT_NAME + "/index";
	}
	
	@RequestMapping(value = {"/{lang}/**",})
	public String subPageLang(ModelMap model, @PathVariable("lang") String language, 
			HttpServletRequest request, HttpServletResponse response) {
		
		resolveLocale(language, request, response);
		addCommonContextVars(model, request);
		return AbstractAdminController.ROOTCONTEXT_NAME + "/index";
	}
}
