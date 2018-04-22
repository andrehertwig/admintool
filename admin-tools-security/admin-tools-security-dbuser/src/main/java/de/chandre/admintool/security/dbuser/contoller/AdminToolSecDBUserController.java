package de.chandre.admintool.security.dbuser.contoller;

import java.io.IOException;
import java.util.Arrays;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.controller.AbstractAdminController;
import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.core.utils.ReflectUtils;
import de.chandre.admintool.security.commons.LoginController;
import de.chandre.admintool.security.commons.TemplateUserService;
import de.chandre.admintool.security.commons.auth.UserStateType;
import de.chandre.admintool.security.commons.auth.UserTO;
import de.chandre.admintool.security.dbuser.auth.ExtUserTO;
import de.chandre.admintool.security.dbuser.domain.ATUser;
import de.chandre.admintool.security.dbuser.service.AdminToolSecDBUserDetailsService;

/**
 * User controller
 * @author André
 * @since 1.1.7
 *
 */
@Controller
@RequestMapping(AdminTool.ROOTCONTEXT + "/accessmanagement/user")
public class AdminToolSecDBUserController extends AbstractAdminController {

	private static final Log LOGGER = LogFactory.getLog(AdminToolSecDBUserController.class);
	
	@Autowired
	private AdminToolSecDBUserDetailsService userService;
	
	@Autowired
	private TemplateUserService templateUserService;
	
	@RequestMapping(value="/profile")
	public String profile(ModelMap model, HttpServletRequest request, HttpServletResponse response) throws IOException {
		if (templateUserService.isAnonymous()) {
			response.sendRedirect(super.getRootContext(request) + LoginController.LOGIN_PATH);
		}
		addCommonContextVars(model, request);
		ATUser currentUser = templateUserService.getUserPrincipal(ATUser.class);
		model.put("currentUser", userService.getUserForId(currentUser.getId()));
		return AdminTool.ROOTCONTEXT_NAME + "/security/content/profile";
	}
	
	@RequestMapping(value="/get/{userId}", method=RequestMethod.GET)
	@ResponseBody
	public ExtUserTO addUser(@PathVariable("userId") String userId) {
		
		ATUser user = userService.getUserForId(userId);
		ExtUserTO output = new ExtUserTO();
		output = ReflectUtils.copyFields(ATUser.class, user, output, false, Arrays.asList("password"), true);
		
		output.setAuthorities(user.getUserGroupNames());
		output.setClients(user.getClientNames());
		output.setActiveRoles(user.getActiveAuthorityNames());
		return output;
	}
	
	@RequestMapping(value="/changeState/{type}", method=RequestMethod.POST)
	@ResponseBody
	public String changeUserState(@RequestBody UserTO userTo, @PathVariable("type") String type) {
		return setUserState(userTo, type);
	}
	
	@RequestMapping(value="/add", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> addUser(@RequestBody UserTO userTo) {
		
		return userService.createUser(userTo);
	}
	
	@RequestMapping(value="/update", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> updateUser(@RequestBody UserTO userTo) {
		return userService.updateUser(userTo);
	}
	
	@RequestMapping(value="/remove", method=RequestMethod.POST)
	@ResponseBody
	public String removeUser(@RequestBody UserTO userTo) {
		try {
			userService.removeByName(userTo.getUsername());
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return Boolean.FALSE.toString();
		}
		return Boolean.TRUE.toString();
	}
	
	protected String setUserState(UserTO userTo, String type) {
		return setUserState(userTo, UserStateType.fromType(type));
	}
	
	/**
	 * 
	 * @param userTo the user transfer object
	 * @param type the user state type to change
	 * @return
	 */
	protected String setUserState(UserTO userTo, UserStateType type) {
		switch (type) {
			case ENABLE:
				userService.setUserEnabled(userTo.getUsername(), userTo.getNewState());
				return "reload";
			case EXIPRE_CREDENTIALS:
				userService.setUserCredentialsExpired(userTo.getUsername(), userTo.getNewState());
				break;
			case EXPIRE:
				userService.setUserExpired(userTo.getUsername(), userTo.getNewState());
				break;
			case LOCK:
				userService.setUserLocked(userTo.getUsername(), userTo.getNewState());
				break;
	
			default:
				return Boolean.FALSE.toString();
		}
		return Boolean.TRUE.toString();
	}
}
