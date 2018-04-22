package de.chandre.admintool.security;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Properties;

import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.springframework.util.CollectionUtils;

import de.chandre.admintool.security.commons.auth.LoginAttemptService;
import de.chandre.admintool.security.simple.auth.AdminToolSecSimpleUserDetailsService;

/**
 * Extension of Springs {@link InMemoryUserDetailsManager} for additional functionality 
 * provided by {@link AdminToolSecSimpleUserDetailsService} interface<br>
 * <br>
 * Usage in Spring Security config:<br>
 * <code>
 * AdminToolInMemoryUserDetailsConfigurer<AuthenticationManagerBuilder> imdmc = <br>
 * &nbsp;&nbsp;new AdminToolInMemoryUserDetailsConfigurer<>(userDetailsService);<br>
 *     imdmc.withUser("operator").password("operator").roles("OPERATOR");<br>
 *     imdmc.withUser("admin").password("admin").roles("ADMIN", "OPERATOR");<br>
 *     <br>
 *     imdmc.configure(auth);
 * 
 * </code>
 * 
 * @author Andre
 * @since 1.0.3
 */
public class AdminToolInMemoryUserDetailsService extends InMemoryUserDetailsManager
	implements AdminToolSecSimpleUserDetailsService {
	
	private List<String> users = new ArrayList<>();
	
	private LoginAttemptService loginAttemptService;
	
	private AdminToolAuthManagerWrapper authenticationManager;
	
	private String infoMessage;
	
	public AdminToolInMemoryUserDetailsService() {
		super(new ArrayList<>());
	}

	public AdminToolInMemoryUserDetailsService(Collection<UserDetails> users) {
		super(users);
	}
	
	public AdminToolInMemoryUserDetailsService(Properties users) {
		super(users);
	}
	
	public String getInfoMessage() {
		return infoMessage;
	}

	public void setInfoMessage(String infoMessage) {
		this.infoMessage = infoMessage;
	}

	@Override
	public void createUser(UserDetails user) {
		if (null == users) {
			users = new ArrayList<>();
		}
		users.add(user.getUsername().toLowerCase(Locale.ENGLISH));
		super.createUser(user);
	}
	
	@Override
	public void deleteUser(String username) {
		if (null != users) {
			users.remove(username.toLowerCase(Locale.ENGLISH));
		}
		super.deleteUser(username);
	}
	
	@Override
	public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
		UserDetails user = super.loadUserByUsername(username);
		
		boolean nonLocked = user.isAccountNonLocked();
		if (null != loginAttemptService) {
			nonLocked = !loginAttemptService.isBlocked(username);
		}
		
		return new User(user.getUsername(), user.getPassword(), user.isEnabled(),
				user.isAccountNonExpired(), user.isCredentialsNonExpired(),
				nonLocked, user.getAuthorities());
	}
	
	@Override
	public void setAuthenticationManager(AuthenticationManager authenticationManager) {
		this.authenticationManager = new AdminToolAuthManagerWrapper(authenticationManager);
		super.setAuthenticationManager(this.authenticationManager);
	}

	public LoginAttemptService getLoginAttemptService() {
		return loginAttemptService;
	}

	public void setLoginAttemptService(LoginAttemptService loginAttemptService) {
		this.loginAttemptService = loginAttemptService;
	}

	@Override
	public Collection<UserDetails> getUsers() {
		List<UserDetails> loadedUsers = new ArrayList<>(users.size());
		for (String username : users) {
			loadedUsers.add(loadUserByUsername(username));
		}
		return loadedUsers;
	}

	@Override
	public void setUserName(String currentUsername, String newUserName) {
		
		UserDetails user = super.loadUserByUsername(currentUsername);
		updateUser(new User(newUserName, user.getPassword(), user.isEnabled(),
				user.isAccountNonExpired(), user.isCredentialsNonExpired(),
				user.isAccountNonLocked(), user.getAuthorities()));
	}

	@Override
	public void setUserLocked(String username, boolean locked) {
		
		UserDetails user = super.loadUserByUsername(username);
		updateUser(new User(user.getUsername(), user.getPassword(), user.isEnabled(),
				user.isAccountNonExpired(), user.isCredentialsNonExpired(),
				!locked, user.getAuthorities()));	
	}

	@Override
	public void setUserExpired(String username, boolean expired) {
		
		UserDetails user = super.loadUserByUsername(username);
		updateUser(new User(user.getUsername(), user.getPassword(), user.isEnabled(),
				!expired, user.isCredentialsNonExpired(),
				user.isAccountNonLocked(), user.getAuthorities()));
	}

	@Override
	public void setUserEnabled(String username, boolean enabled) {

		UserDetails user = super.loadUserByUsername(username);
		updateUser(new User(user.getUsername(), user.getPassword(), enabled,
				user.isAccountNonExpired(), user.isCredentialsNonExpired(),
				user.isAccountNonLocked(), user.getAuthorities()));
	}

	@Override
	public void setUserCredentialsExpired(String username, boolean credentialsExpired) {

		UserDetails user = super.loadUserByUsername(username);
		updateUser(new User(user.getUsername(), user.getPassword(), user.isEnabled(),
				!credentialsExpired, user.isCredentialsNonExpired(),
				user.isAccountNonLocked(), user.getAuthorities()));
	}
	
	@Override
	public void setUserRoles(String username, Collection<GrantedAuthority> authorities) {
		if (!CollectionUtils.isEmpty(authorities)) {
			UserDetails user = super.loadUserByUsername(username);
			
			updateUser(new User(user.getUsername(), user.getPassword(), user.isEnabled(),
					user.isCredentialsNonExpired(), user.isCredentialsNonExpired(),
					user.isAccountNonLocked(), authorities));
		}
	}

	@Override
	public void addUserRoles(String username, Collection<GrantedAuthority> authorities) {
		
		if (!CollectionUtils.isEmpty(authorities)) {
			UserDetails user = super.loadUserByUsername(username);
			Collection<GrantedAuthority> extAuthorities = new HashSet<>(user.getAuthorities());
			extAuthorities.addAll(authorities);
			
			updateUser(new User(user.getUsername(), user.getPassword(), user.isEnabled(),
					user.isCredentialsNonExpired(), user.isCredentialsNonExpired(),
					user.isAccountNonLocked(), extAuthorities));
		}
	}

	@Override
	public void removeUserRoles(String username, Collection<GrantedAuthority> authorities) {
		
		if (!CollectionUtils.isEmpty(authorities)) {
			UserDetails user = super.loadUserByUsername(username);
			Collection<GrantedAuthority> extAuthorities = new HashSet<>(user.getAuthorities());
			Iterator<GrantedAuthority> extAuthIter = extAuthorities.iterator();
			while (extAuthIter.hasNext()) {
				GrantedAuthority extGrantedAuthority = (GrantedAuthority) extAuthIter.next();
				if (authorities.contains(extGrantedAuthority)) {
					extAuthIter.remove();
				}
			}
						
			updateUser(new User(user.getUsername(), user.getPassword(), user.isEnabled(),
					user.isCredentialsNonExpired(), user.isCredentialsNonExpired(),
					user.isAccountNonLocked(), extAuthorities));
		}
	}
}
