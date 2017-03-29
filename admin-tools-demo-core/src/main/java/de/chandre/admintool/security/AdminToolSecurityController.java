package de.chandre.admintool.security;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.security.auth.AbstractAdminToolSecurityViewController;
import de.chandre.admintool.core.security.auth.AdminToolUserDetailsService;
import de.chandre.admintool.core.security.auth.UserTO;

@Controller
@RequestMapping(AdminTool.ROOTCONTEXT)
public class AdminToolSecurityController extends AbstractAdminToolSecurityViewController {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolSecurityController.class);
	
	@Autowired
	private AdminToolInMemoryUserDetailsService userDetailsService;
	
	@Override
	protected AdminToolUserDetailsService getUserServiceDelegate() {
		return userDetailsService;
	}
	
	@RequestMapping(value="/users/roles", method=RequestMethod.GET)
	@ResponseBody
	public Collection<String> getUserRoles() {
		Collection<String> roles = new ArrayList<String>();
		roles.add("ROLE_ADMIN");
		roles.add("ROLE_OPERATOR");
		roles.add("ROLE_USER");
		return roles;
	}
	
	@RequestMapping(value="/users/changeState/{type}", method=RequestMethod.POST)
	@ResponseBody
	public String changeUserState(@RequestBody UserTO userTo, @PathVariable("type") String type) {
		return setUserState(userTo, type);
	}
	
	
	@RequestMapping(value="/users/addUser", method=RequestMethod.POST)
	@ResponseBody
	public String addUser(@RequestBody UserTO userTo) {
		
		LOGGER.debug("addUser: " + userTo.toString());
		
		if (StringUtils.isBlank(userTo.getPassword())) {
			return "Passwort must be empty";
		}
		String check = checkPassword(userTo.getPassword());
		if (null != check) {
			return check;
		}
		
		userDetailsService.createUser(new User(userTo.getUsername(), userTo.getPassword(), false,
				true, true,
				true, transformToSimpleAuthorities(userTo.getAuthorities())));
		
		return Boolean.TRUE.toString();
	}
	
	@RequestMapping(value="/users/updateUser", method=RequestMethod.POST)
	@ResponseBody
	public String updateUser(@RequestBody UserTO userTo) {
		
		LOGGER.debug("updateUser: " + userTo.toString());
		
		UserDetails user = userDetailsService.loadUserByUsername(userTo.getUsername());
		if (null == user) {
			return String.format("no user with name '%s' found", userTo.getUsername());
		}
		
		String pwd = user.getPassword();
		if (StringUtils.isNotBlank(userTo.getPassword())) {
			pwd = userTo.getPassword();
			String check = checkPassword(pwd);
			if (null != check) {
				return check;
			}
		}
		
		userDetailsService.updateUser(new User(user.getUsername(), pwd, user.isEnabled(),
				user.isCredentialsNonExpired(), user.isCredentialsNonExpired(),
				user.isAccountNonLocked(), transformToSimpleAuthorities(userTo.getAuthorities())));
		
		return Boolean.TRUE.toString();
	}
	
	private String checkPassword(String password) {
		if (password.length() < 5) {
			return "Passwort must have minimum length of 5";
		}
		return null;
	}
	
}
