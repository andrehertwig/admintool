package de.chandre.admintool.security.dbuser.contoller;

import java.io.IOException;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.commons.LoginController;
import de.chandre.admintool.security.commons.TemplateUserService;
import de.chandre.admintool.security.commons.auth.UserTO;
import de.chandre.admintool.security.dbuser.AdminToolSecDBProperties;
import de.chandre.admintool.security.dbuser.AdminToolSecDBTemplateUtils;
import de.chandre.admintool.security.dbuser.Constants;
import de.chandre.admintool.security.dbuser.Constants.CommunicationProcess;
import de.chandre.admintool.security.dbuser.auth.PasswordTO;
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
@RequestMapping(AdminTool.ROOTCONTEXT + "/accessmanagement/user/profile")
public class AdminToolSecDBProfileController extends ATSecDBAbctractController {
	
	private final Log LOGGER = LogFactory.getLog(AdminToolSecDBProfileController.class);
	
	@Autowired 
	private AdminToolSecDBProperties properties;
	
	@Autowired
	private TemplateUserService templateUserService;
	
	@Autowired
	private AdminToolSecDBUserDetailsService userService;
	
	@Autowired
	private AdminToolSecDBUserValidator userValidator;
	
	@Autowired
	private AdminToolSecDBTemplateUtils templateUtils;
	
	@RequestMapping(value= {"", "/"})
	public String profile(ModelMap model, HttpServletRequest request, HttpServletResponse response) throws IOException {
		if (templateUserService.isAnonymous()) {
			response.sendRedirect(super.getRootContext(request) + LoginController.LOGIN_PATH);
		}
		addCommonContextVars(model, request);
		ATUser currentUser = templateUserService.getUserPrincipal(ATUser.class);
		model.put("currentUser", userService.getUserForId(currentUser.getId()));
		return AdminTool.ROOTCONTEXT_NAME + Constants.CONTENT_TPL_PATH + "profile";
	}
	
	@RequestMapping(value="/update", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> profileUpdate(@RequestBody UserTO userTO, HttpServletRequest request, HttpServletResponse response) throws IOException {
		LOGGER.debug("receiving profile/update request");
		if (templateUserService.isAnonymous()) {
			return null;
		}
		
		try {
			userTO.setUsername(templateUserService.getUserName());
			Set<ATError> errors = userService.updateProfile(userTO);
			return errors;
		} catch (Exception e) {
			return handleException(e, LOGGER, "error.update.profile", 
					Constants.MSG_KEY_PREFIX + "profile.update.profile.error", "Could not update profile");
		}
	}
	
	@RequestMapping(value="/password/update", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> updatePassword(@RequestBody PasswordTO userTO, HttpServletRequest request, HttpServletResponse response) throws IOException {
		LOGGER.debug("receiving  profile/password/update request");
		if (!properties.getUsers().isDirectPasswordChangeInProfileAllowed() || templateUserService.isAnonymous()) {
			return null;
		}
		
		try {
			String userName = templateUserService.getUserName();
			Set<ATError> errors = userValidator.validatePasswordChange(userName, userTO.getCurrentPassword(), 
					userTO.getNewPassword(), userTO.getPasswordConfirm());
			
			if(CollectionUtils.isEmpty(errors)) {
				userService.updatePassword(userName, userTO.getNewPassword());
			}
			return errors;
		} catch (Exception e) {
			return handleException(e, LOGGER, "error.update.password", 
					Constants.MSG_KEY_PREFIX + "profile.update.password.error", "Could not update password");
		}
	}
	
	@RequestMapping(value="/password/reset", method=RequestMethod.POST)
	@ResponseBody
	public String resetPassword( HttpServletRequest request, HttpServletResponse response) throws IOException {
		LOGGER.debug("receiving  profile/password/reset request");
		if (!templateUtils.isCommunicatorImplemented() || templateUserService.isAnonymous()) {
			return Boolean.FALSE.toString();
		}
		try {
			userService.createResetPassword(templateUserService.getUserName(), CommunicationProcess.RESET_PASSWORD_REQUEST_SELF);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return Boolean.FALSE.toString();
		}
		return Boolean.TRUE.toString();
	}
	
}
