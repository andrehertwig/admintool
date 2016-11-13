package de.chandre.admintool.core.security;

import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.RequestMapping;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.controller.AbstractAdminController;

/**
 * Controller for login page
 * @author Andre
 * @since 1.0.1
 */
@Controller
@RequestMapping(AdminTool.ROOTCONTEXT)
public class LoginController extends AbstractAdminController {

	@RequestMapping(value = {"/login","/login/**"})
	public String login(ModelMap model, HttpServletRequest request) {
		addCommonContextVars(model, request);
		model.put("error", null != request.getParameter("error"));
		return AdminTool.ROOTCONTEXT_NAME + "/login";
	}
}
