package de.chandre.admintool.security.simple.auth;

import java.util.Collection;

import org.springframework.security.core.GrantedAuthority;

import de.chandre.admintool.security.commons.auth.AdminToolUserDetailsService;

/**
 * Interface for admintool UserDetailsService 
 * @author Andre
 * @since 1.1.5
 */
public interface AdminToolSecSimpleUserDetailsService extends AdminToolUserDetailsService {
	
	/**
	 * add roles to a user
	 * @param username
	 * @param authorities
	 */
	void addUserRoles(String username, Collection<GrantedAuthority> authorities);
	
	/**
	 * remove roles from user
	 * @param username
	 * @param authorities
	 */
	void removeUserRoles(String username, Collection<GrantedAuthority> authorities);

	/**
	 * override all roles
	 * @param username
	 * @param authorities
	 */
	void setUserRoles(String username, Collection<GrantedAuthority> authorities);
}
