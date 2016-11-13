package de.chandre.admintool.core.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.AdminToolCoreConfig;
import de.chandre.admintool.core.utils.ExceptionUtils;

/**
 * the root admin controller serving templates generally
 * @author Andre
 *
 */
@Controller
@RequestMapping(AdminTool.ROOTCONTEXT)
public class AdminRootController extends AbstractAdminController
{
	private static final Log LOGGER = LogFactory.getLog(AdminRootController.class);
	
	@Autowired
	private AdminToolCoreConfig conig;
	
	@RequestMapping(value = {"", "/"})
	public String startPage(ModelMap model, HttpServletRequest request) {
		
		if(LOGGER.isTraceEnabled()) LOGGER.trace("serving admin root page");
		model.put("rootContext", request.getContextPath() + AdminTool.ROOTCONTEXT);
		model.put("contentPage", "admintool/content/start");
		return AdminTool.ROOTCONTEXT_NAME + "/content/start";
	}
	
	@RequestMapping(value = {"/**"})
	public String subPage(ModelMap model, HttpServletRequest request) {
		
		String targetTpl = addCommonContextVars(model, request);
		return AdminTool.ROOTCONTEXT_NAME + AdminTool.SLASH + targetTpl;
	}
	
	@RequestMapping(value = {"/{lang}/**"})
	public String subPageLang(ModelMap model, @PathVariable("lang") String language, 
			HttpServletRequest request, HttpServletResponse response) {
		
		resolveLocale(language, request, response);
		String targetTpl = addCommonContextVars(model, request);
		return AdminTool.ROOTCONTEXT_NAME + AdminTool.SLASH + targetTpl;
	}
	
	@ExceptionHandler(Exception.class)
	public ModelAndView handleException(Exception exception, HttpServletRequest request) {
		if(LOGGER.isTraceEnabled()) LOGGER.trace("handleException: " + exception.getMessage());
		
		ModelAndView mv = new ModelAndView(AdminTool.GENERIC_ERROR_TPL_PATH);
		mv.getModelMap().put("exceptionMessage", exception.getMessage());
//		mv.getModelMap().put("httpStatus", response.getStatus());
//		HttpStatus status = HttpStatus.valueOf(response.getStatus());
//		mv.getModelMap().put("httpStatusMessage", status != null ? status.getReasonPhrase() : "");
		
		mv.getModelMap().put("showStacktrace", conig.isShowStacktraceOnErrorPage());
		mv.getModelMap().put("stacktrace", ExceptionUtils.printException(exception));
		return mv;
	}
}
