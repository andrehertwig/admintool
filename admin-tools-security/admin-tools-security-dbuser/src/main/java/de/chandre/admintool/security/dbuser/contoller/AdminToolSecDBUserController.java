package de.chandre.admintool.security.dbuser.contoller;

import java.util.Arrays;
import java.util.Collections;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.core.utils.ReflectUtils;
import de.chandre.admintool.security.commons.auth.UserStateType;
import de.chandre.admintool.security.commons.auth.UserTO;
import de.chandre.admintool.security.dbuser.Constants.CommunicationProcess;
import de.chandre.admintool.security.dbuser.auth.ExtUserTO;
import de.chandre.admintool.security.dbuser.domain.ATUser;
import de.chandre.admintool.security.dbuser.service.ATSecDBUserControllerAuthProxy;
import de.chandre.admintool.security.dbuser.service.comm.SendException;
import de.chandre.admintool.security.dbuser.service.validation.AdminToolSecDBUserValidator;

/**
 * User controller
 * @author André
 * @since 1.2.0
 *
 */
@Controller
@RequestMapping(AdminTool.ROOTCONTEXT + "/accessmanagement/user")
public class AdminToolSecDBUserController extends ATSecDBAbctractController {

	private final Log LOGGER = LogFactory.getLog(AdminToolSecDBUserController.class);
	
	@Autowired
	private ATSecDBUserControllerAuthProxy userService;

	@Autowired
	private AdminToolSecDBUserValidator userValidator;
	
	@RequestMapping(value="/get/{userId}", method=RequestMethod.GET)
	@ResponseBody
	public ExtUserTO addUser(@PathVariable("userId") String userId) {
		try {
			ATUser user = userService.getUserForId(userId);
			ExtUserTO output = new ExtUserTO();
			output = ReflectUtils.copyFields(ATUser.class, user, output, false, Arrays.asList("password"), true);
			
			output.setAuthorities(user.getUserGroupNames());
			output.setClients(user.getClientNames());
			output.setActiveRoles(user.getActiveAuthorityNames());
			return output;
		} catch (Exception e) {
			//TODO: handle error in ui
			LOGGER.error(e.getMessage(), e);
		}
		return null;
	}
	
	@RequestMapping(value="/changeState/{type}", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> changeUserState(@RequestBody UserTO userTo, @PathVariable("type") String type) {
		try {
			if(setUserState(userTo, type)) {
				return Collections.emptySet();
			}
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return handleException(e, LOGGER, userValidator, "error.update.state.group", "update.state.group.error", "Could not change UserGroup state");
		}
		return super.createError(this.userValidator, "error.update.state.group", "update.state.group.error", "Could not change UserGroup state for group: ", userTo.getUsername());
	
	}
	
	@RequestMapping(value="/add", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> addUser(@RequestBody UserTO userTo) {
		
		try {
			return userService.createUser(userTo);
		} catch (SendException e) {
			LOGGER.error(e.getMessage(), e);
			return handleException(e, LOGGER, userValidator, "error.create.user", "create.user.error", "Could not create user");
		}
	}
	
	@RequestMapping(value="/update", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> updateUser(@RequestBody UserTO userTo) {
		try {
			return userService.updateUser(userTo);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return handleException(e, LOGGER, userValidator, "error.update.user", "update.user.error", "Could not update user");
		}
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
	
	@RequestMapping(value="/resetPassword", method=RequestMethod.POST)
	@ResponseBody
	public String resetPassword(@RequestBody UserTO userTo) {
		try {
			userService.createResetPassword(userTo.getUsername(), CommunicationProcess.RESET_PASSWORD_REQUEST_ADMIN);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return Boolean.FALSE.toString();
		}
		return Boolean.TRUE.toString();
	}
	
	protected boolean setUserState(UserTO userTo, String type) {
		return setUserState(userTo, UserStateType.fromType(type));
	}
	
	/**
	 * 
	 * @param userTo the user transfer object
	 * @param type the user state type to change
	 * @return
	 */
	protected boolean setUserState(UserTO userTo, UserStateType type) {
		switch (type) {
			case ENABLE:
				userService.setUserEnabled(userTo.getUsername(), userTo.getNewState());
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
				return Boolean.FALSE.booleanValue();
		}
		return Boolean.TRUE.booleanValue();
	}
}
