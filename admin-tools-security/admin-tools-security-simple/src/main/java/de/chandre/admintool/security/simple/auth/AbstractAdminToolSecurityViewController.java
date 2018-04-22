package de.chandre.admintool.security.simple.auth;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.util.StringUtils;

import de.chandre.admintool.core.controller.AbstractAdminController;
import de.chandre.admintool.security.commons.auth.UserStateType;
import de.chandre.admintool.security.commons.auth.UserTO;

/**
 * Abstract controller for security view.
 * <br><br>
 * There should be some methods to get it work... of course you can implement your own solution.<br>
 * <pre>
 * {@literal @}RequestMapping(value="/users/roles", method=RequestMethod.GET)
 * {@literal @}ResponseBody
 * public Collection<String> getUserRoles() {
 *    //shoud return a collection of roles .. but it's optional
 * }
 * 
 * {@literal @}RequestMapping(value="/users/changeState/{type}", method=RequestMethod.POST)
 * {@literal @}ResponseBody
 * public String changeUserState(@RequestBody UserTO userTo, @PathVariable("type") String type) {
 *     return setUserState(userTo, type);
 * }
 * 
 * {@literal @}RequestMapping(value="/users/addUser", method=RequestMethod.POST)
 * {@literal @}ResponseBody
 * public String addUser(@RequestBody UserTO userTo) {
 *    //your implementation to add a new user
 * }
 * 
 * {@literal @}RequestMapping(value="/users/updateUser", method=RequestMethod.POST)
 * {@literal @}ResponseBody
 * public String updateUser(@RequestBody UserTO userTo) {
 *    //your implementation to update a existing user
 * }
 * </pre>
 * 
 * @author Andre
 * @since 1.1.5
 */
public abstract class AbstractAdminToolSecurityViewController extends AbstractAdminController {
	
	private static final String ROLE_PREFIX = "ROLE_";
	
	/**
	 * should return the instance of {@link UserDetailsService}
	 * @return
	 */
	protected abstract AdminToolSecSimpleUserDetailsService getUserServiceDelegate();
	
	/**
	 * @see #setUserState(UserTO, UserStateType)
	 * @param userTo
	 * @param type
	 * @return
	 */
	protected String setUserState(UserTO userTo, String type) {
		return setUserState(userTo, UserStateType.fromType(type));
	}
	
	/**
	 * override this for another prefix. should end with underscore.
	 * @return
	 */
	protected static String getRolePrefix() {
		return ROLE_PREFIX;
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
				getUserServiceDelegate().setUserEnabled(userTo.getUsername(), userTo.getNewState());
				break;
			case EXIPRE_CREDENTIALS:
				getUserServiceDelegate().setUserCredentialsExpired(userTo.getUsername(), userTo.getNewState());
				break;
			case EXPIRE:
				getUserServiceDelegate().setUserExpired(userTo.getUsername(), userTo.getNewState());
				break;
			case LOCK:
				getUserServiceDelegate().setUserLocked(userTo.getUsername(), userTo.getNewState());
				break;
	
			default:
				return Boolean.FALSE.toString();
		}
		return Boolean.TRUE.toString();
	}

	/**
	 * @see #transformToSimpleAuthorities(Set, boolean)
	 * @param strAuthorities
	 * @return Empty collection or collection of {@link SimpleGrantedAuthority}
	 */
	protected Collection<GrantedAuthority> transformToSimpleAuthorities(Set<String> strAuthorities) {
		return transformToSimpleAuthorities(strAuthorities, false);
	}
	
	/**
	 * transforms all authorities to upper case and append the prefix if appendRolePrefix = true
	 *  
	 * @param strAuthorities
	 * @param appendRolePrefix
	 * @return Empty collection or collection of {@link SimpleGrantedAuthority}
	 */
	protected Collection<GrantedAuthority> transformToSimpleAuthorities(Set<String> strAuthorities, boolean appendRolePrefix) {
		if (null != strAuthorities) {
			Collection<GrantedAuthority> authorities = new HashSet<>(strAuthorities.size());
			for (String authority : strAuthorities) {
				if (!StringUtils.isEmpty(authority)) {
					String role = authority.trim().toUpperCase(Locale.ENGLISH);
					if (appendRolePrefix) {
						authorities.add(new SimpleGrantedAuthority(getRolePrefix() + role));
					} else {
						authorities.add(new SimpleGrantedAuthority(role));
					}
				}
			}
			return authorities;
		}
		return Collections.emptyList();
	}
}
