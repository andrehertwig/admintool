package de.chandre.admintool.core.security;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.RequestMapping;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.AdminToolCoreConfig;
import de.chandre.admintool.core.controller.AbstractAdminController;

/**
 * Controller for login page
 * @author Andre
 * @since 1.0.1
 */
@Controller
@RequestMapping(AdminTool.ROOTCONTEXT)
public class LoginController extends AbstractAdminController {
	
	private static final Log LOGGER = LogFactory.getLog(LoginController.class);
	
	@Autowired
	private AdminToolCoreConfig config;

	@RequestMapping(value = {"/login","/login/**"})
	public String login(ModelMap model, HttpServletRequest request) {
		addCommonContextVars(model, request);
		if (!config.isEnabled()) {
			return AdminTool.GENERIC_DEACTIVATED_TEMPLATE_TPL_PATH;
		}
		boolean error = null != request.getParameter("error");
		model.put("error", error);
		LOGGER.debug("login called with error: " + (error ? "true" : "false"));
		return AdminTool.ROOTCONTEXT_NAME + "/login";
	}
}