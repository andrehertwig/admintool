package de.chandre.admintool.security.dbuser.contoller;

import java.io.IOException;
import java.time.Period;
import java.time.ZonedDateTime;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.controller.AbstractAdminController;
import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.commons.LoginController;
import de.chandre.admintool.security.dbuser.AdminToolSecDBProperties;
import de.chandre.admintool.security.dbuser.Constants;
import de.chandre.admintool.security.dbuser.Constants.CommunicationProcess;
import de.chandre.admintool.security.dbuser.domain.ATUser;
import de.chandre.admintool.security.dbuser.service.AdminToolSecDBUserDetailsService;
import de.chandre.admintool.security.dbuser.service.validation.AdminToolSecDBUserValidator;

/**
 * 
 * @author André
 * @since 1.2.0
 *
 */
@Controller
@RequestMapping(AdminTool.ROOTCONTEXT + "/accessmanagement/reset")
public class AdminToolSecDBPublicController extends AbstractAdminController {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolSecDBUserController.class);
	
	@Autowired 
	private AdminToolSecDBProperties properties;
	
	@Autowired
	private AdminToolSecDBUserDetailsService userService;
	
	@Autowired
	private AdminToolSecDBUserValidator userValidator;
	
	@RequestMapping(value="/password", method=RequestMethod.GET)
	public String createResetPasswordPage(ModelMap model, HttpServletRequest request, HttpServletResponse response) throws IOException {
		addCommonContextVars(model, request);
		model.put("init", true);
		model.put("success", false);
		return AdminTool.ROOTCONTEXT + Constants.CONTENT_TPL_PATH + "resetPasswordRequest";
	}
	
	@RequestMapping(value="/password/request", method=RequestMethod.POST)
	public String createResetPassword(@RequestParam("username") String username, 
			ModelMap model, HttpServletRequest request, HttpServletResponse response) throws IOException {
		LOGGER.debug("receiving resetpassword/create request for user: " + username);
		addCommonContextVars(model, request);
		model.put("init", false);
		try {
			userService.createResetPassword(username, CommunicationProcess.RESET_PASSWORD_REQUEST_SELF);
			model.put("success", true);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			model.put("success", false);
		}
		return AdminTool.ROOTCONTEXT + Constants.CONTENT_TPL_PATH + "resetPasswordRequest";
	}
	
	@RequestMapping(value="/password/{hash}", method=RequestMethod.GET)
	public String resetPasswordCheck(@PathVariable("hash") String passwordLinkHash, ModelMap model, HttpServletRequest request, HttpServletResponse response) throws IOException {
		LOGGER.debug("receiving /password/{hash} request for hash: " + passwordLinkHash);
		ATUser user = userService.getUserForPasswordHash(passwordLinkHash);
		if (null == user) {
			LOGGER.warn(String.format("reset password for hash %s requested, but no user found", passwordLinkHash));
			response.sendRedirect(super.getRootContext(request) + LoginController.LOGIN_PATH);
		}
		
		Period hashPeriod = properties.getUsers().getPasswordHashPeriod();
		ZonedDateTime maxDate = user.getPasswordLinkCreated().plus(hashPeriod);
		if (ZonedDateTime.now().isAfter(maxDate)) {
			LOGGER.warn(String.format("reset password for hash %s and user %s is expired", passwordLinkHash, user.getUsername()));
			response.sendRedirect(super.getRootContext(request) + LoginController.LOGIN_PATH + "?error=hashToOld");
		}
		
		addCommonContextVars(model, request);
		model.put("passwordLinkHash", passwordLinkHash);
		model.put("success", false);
		model.put("init", true);
		return AdminTool.ROOTCONTEXT + Constants.CONTENT_TPL_PATH + "resetPassword";
	}
	
	@RequestMapping(value="/password/update", method=RequestMethod.POST)
	public String resetPassword(
			@RequestParam("passwordLinkHash")String passwordLinkHash,
			@RequestParam("username") String username,
			@RequestParam("password") String password,
			@RequestParam("passwordConfirm")String passwordConfirm,
			ModelMap model, HttpServletRequest request, HttpServletResponse response) throws IOException {
		LOGGER.debug("receiving /password request for user " + username +" hash: " + passwordLinkHash);
		ATUser userByHash = userService.getUserForPasswordHash(passwordLinkHash);
		ATUser userByName = userService.getUser(username);
		
		if (null == userByHash || null == userByName || !userByHash.getUsername().equals(userByName.getUsername())) {
			LOGGER.warn(String.format("reset password for hash %s and user %s is not equals given user name %s, ", 
					passwordLinkHash, userByHash.getUsername(), userByName.getUsername()));
			response.sendRedirect(super.getRootContext(request) + LoginController.LOGIN_PATH+ "?error=systemAdmin");
		}
		
		addCommonContextVars(model, request);
		model.put("passwordLinkHash", passwordLinkHash);
		model.put("username", username);
		model.put("init", false);
		try {
			Set<ATError> errors = userValidator.validatePasswordReset(username, password, passwordConfirm);
			
			if (CollectionUtils.isEmpty(errors)) {
				userService.resetPassword(username, password);
				model.put("success", true);
			} else {
				model.put("success", false);
				model.put("errors", errors);
			}
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			model.put("success", false);
		}
		return AdminTool.ROOTCONTEXT + Constants.CONTENT_TPL_PATH + "resetPassword";
	}
}
