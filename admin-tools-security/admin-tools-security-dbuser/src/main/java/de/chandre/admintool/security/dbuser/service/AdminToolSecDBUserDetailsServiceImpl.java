package de.chandre.admintool.security.dbuser.service;

import java.time.LocalDateTime;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.security.commons.auth.LoginAttemptService;
import de.chandre.admintool.security.commons.auth.UserTO;
import de.chandre.admintool.security.dbuser.Constants;
import de.chandre.admintool.security.dbuser.domain.ATClient;
import de.chandre.admintool.security.dbuser.domain.ATUser;
import de.chandre.admintool.security.dbuser.domain.ATUserGroup;
import de.chandre.admintool.security.dbuser.domain.User;
import de.chandre.admintool.security.dbuser.repo.ClientRepository;
import de.chandre.admintool.security.dbuser.repo.UserGroupRepository;
import de.chandre.admintool.security.dbuser.repo.UserRepository;
import de.chandre.admintool.security.dbuser.service.validation.AdminToolSecDBUserValidator;

/**
 * 
 * @author André
 * @since 1.1.7
 */
@Service("adminToolSecDBUserDetailsService")
@Transactional
public class AdminToolSecDBUserDetailsServiceImpl implements AdminToolSecDBUserDetailsService {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolSecDBUserDetailsServiceImpl.class);
	
	@Autowired
	private UserRepository userRepository;
	
	@Autowired
	private UserGroupRepository userGroupRepository;
	
	@Autowired
	private ClientRepository clientRepository;
	
	@Autowired(required=false)
	private AdminToolSecDBUserValidator validator;
	
	@Autowired(required=false)
	private LoginAttemptService loginAttemptService;
	
	@Autowired(required=false)
	private PasswordEncoder passwordEncoder;
	
	private String infoMessage;
	
	private int maxLoginAttempts = 5;

	@Override
	public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
		ATUser user = getUser(username);
		if (null == user) {
			throw new UsernameNotFoundException("no user found");
		}
		if(user.isNotEnabled()) {
			throw new UsernameNotFoundException("user is not enabled");
		}
		
		boolean locked = user.isAccountLocked();
		if (null != loginAttemptService) {
			locked = loginAttemptService.isBlocked(username);
			setUserLocked(user, locked);
		}
		return user;
	}
	
	@Override
	public ATUser getUser(String username) {
		return userRepository.findByUsername(username);
	}
	
	@Override
	public ATUser getUserForId(String userid) {
		return userRepository.getOne(userid);
	}

	@Override
	public String getInfoMessage() {
		return infoMessage;
	}

	@Override
	public void setInfoMessage(String infoMessage) {
		this.infoMessage = infoMessage;
	}

	@Override
	public Collection<? extends User> getUsers() {
		return this.userRepository.findAll();
	}
	
	@Override
	public ATUser saveUser(ATUser user) {
		return this.userRepository.saveAndFlush(user);
	}
	
	@Override
	public ATUser saveUser(ATUser user, boolean encodePassword) {
		if (!encodePassword) {
			return saveUser(user);
		}
		if (encodePassword && null != passwordEncoder) {
			user.setPassword(passwordEncoder.encode(user.getPassword()));
			return saveUser(user);
		}
		throw new RuntimeException("no password encoder found");
	}

	@Override
	public void setUserName(String currentUsername, String newUserName) {
		setUserName(getUser(currentUsername), newUserName);
	}
	
	@Override
	public ATUser setUserName(ATUser currentUser, String newUserName) {
		if (!currentUser.getUsername().equals(newUserName)) {
			currentUser.setUsername(newUserName);
			currentUser = this.userRepository.saveAndFlush(currentUser);
		}
		return currentUser;
	}

	@Override
	public void setUserLocked(String username, boolean locked) {
		setUserLocked(getUser(username), locked);
	}
	
	@Override
	public ATUser setUserLocked(ATUser currentUser, boolean locked) {
		if (locked != currentUser.isAccountLocked()) {
			if (locked) {
				currentUser.lockAccount();
			} else {
				currentUser.setAccountNonLocked(true);
				currentUser.setAccountLockedSince(null);
			}
			currentUser = this.userRepository.saveAndFlush(currentUser);
		}
		return currentUser;
	}

	@Override
	public void setUserExpired(String username, boolean expired) {
		setUserExpired(getUser(username), expired);
	}
	
	@Override
	public ATUser setUserExpired(ATUser currentUser, boolean expired) {
		if (expired != currentUser.isAccountExpired()) {
			if (expired) {
				currentUser.expireAccount();
			} else {
				currentUser.setAccountNonExpired(false);
				currentUser.setAccountExpiredSince(null);
			}
			currentUser = this.userRepository.saveAndFlush(currentUser);
		}
		return currentUser;
	}

	@Override
	public void setUserEnabled(String username, boolean enabled) {
		setUserEnabled(getUser(username), enabled);
	}
	
	@Override
	public ATUser setUserEnabled(ATUser currentUser, boolean enabled) {
		currentUser.setEnabled(enabled);
		currentUser = this.userRepository.saveAndFlush(currentUser);
		return currentUser;
	}

	@Override
	public void setUserCredentialsExpired(String username, boolean credentialsExpired) {
		setUserCredentialsExpired(getUser(username), credentialsExpired);
	}
	
	@Override
	public ATUser setUserCredentialsExpired(ATUser currentUser, boolean credentialsExpired) {
		if (credentialsExpired != currentUser.isCredentialsExpired()) {
			currentUser.setCredentialsNonExpired(!credentialsExpired);
			if (credentialsExpired) {
				currentUser.expireCredentials();
			} else {
				currentUser.setCredentialsNonExpired(false);
				currentUser.setCredentialsExpiredSince(null);
			}
			currentUser = this.userRepository.saveAndFlush(currentUser);
		}
		return currentUser;
	}
	
	private Set<ATError> setAndValidateAndSave(UserTO userTO, ATUser user, boolean validatePassword) {
		user.setEmail(StringUtils.trimToNull(userTO.getEmail()));
		user.setPhone(StringUtils.trimToNull(userTO.getPhone()));
		user.setFirstName(StringUtils.trimToNull(userTO.getFirstName()));
		user.setLastName(StringUtils.trimToNull(userTO.getLastName()));
		if(!CollectionUtils.isEmpty(userTO.getAuthorities())) {
			user.setUserGroups(this.userGroupRepository.findByNameIn(userTO.getAuthorities()));
		}
		if(!CollectionUtils.isEmpty(userTO.getClients())) {
			user.setClients(this.clientRepository.findByNameIn(userTO.getClients()));
		}
		
		if (user.getTimeZone() == null) {
			if (StringUtils.isBlank(userTO.getTimeZone())) {
				user.setTimeZone(TimeZone.getDefault());
			} else {
				user.setTimeZone(userTO.getTimeZone());
			}
		}
		if (user.getLocale() == null) {
			if (StringUtils.isBlank(userTO.getLocale())) {
				user.setLocale(LocaleContextHolder.getLocale());
			} else {
				user.setLocale(userTO.getLocale());
			}
		}
		
		Set<ATError> errors = Collections.emptySet();
		if (validator != null) {
			errors = validator.validate(user, validatePassword);
		}
		
		if (CollectionUtils.isEmpty(errors)) {
			try {
				saveUser(user, validatePassword);
			} catch (Exception e) {
				LOGGER.debug(e.getMessage(), e);
				errors.add(new ATError(Constants.MSG_KEY_PREFIX + "user.save", 
						validator.getMessageWithSuffix("save", null, "Exception during save"), "generic"));
			}
		}
		return errors;
	}
	
	@Override
	public Set<ATError> createUser(UserTO userTO) {
		Set<ATError> errors = null;
		if (null != getUser(StringUtils.trimToNull(userTO.getUsername()))) {
			errors = new HashSet<>();
			errors.add(new ATError(Constants.MSG_KEY_PREFIX + "user.alreadyExists", 
					validator.getMessageWithSuffix("alreadyExists", null, "The user exists already"), "username"));
			return errors;
		}
		ATUser user = new ATUser(StringUtils.trimToNull(userTO.getUsername()), StringUtils.trimToNull(userTO.getPassword()));
		if (LOGGER.isDebugEnabled()) {
			LOGGER.debug(String.format("creating user: %s", user.getUsername()));
		}
		
		return setAndValidateAndSave(userTO, user, true);
	}
	
	@Override
	public Set<ATError> updateUser(UserTO userTO) {
		Set<ATError> errors = null;
		ATUser user = getUser(userTO.getUsername());
		if (null == user) {
			errors = new HashSet<>();
			errors.add(new ATError(Constants.MSG_KEY_PREFIX + "user.notFound", 
					validator.getMessageWithSuffix("notFound", null, "No user foud"), "username"));
			return errors;
		}
		if (LOGGER.isDebugEnabled()) {
			LOGGER.debug(String.format("updateding user: %s (%s)", user.getUsername(), user.getId()));
		}
		
		boolean passwordChanged= false;
		if (StringUtils.isNotEmpty(userTO.getPassword())) {
			user.setPassword(userTO.getPassword());
			passwordChanged = true;
		}
		
		return setAndValidateAndSave(userTO, user, passwordChanged);
	}
	
	@Override
	public void removeByName(String username) {
		this.userRepository.deleteByUsername(username);
	}
	
	@Override
	public ATUser addUserGroups(ATUser user, Set<String> groupNames) {
		List<ATUserGroup> groups = this.userGroupRepository.findByNameIn(groupNames);
		user.getUserGroups().addAll(groups);
		return this.userRepository.saveAndFlush(user);
	}
	
	@Override
	public ATUser addCients(ATUser user, Set<String> clientNames) {
		List<ATClient> groups = this.clientRepository.findByNameIn(clientNames);
		user.getClients().addAll(groups);
		return this.userRepository.saveAndFlush(user);
	}
	
	@Override
	public ATUser removeUserGroups(ATUser user, Set<String> groupNames) {
		Iterator<ATUserGroup> userGroupsIter = user.getUserGroups().iterator();
		while (userGroupsIter.hasNext()) {
			ATUserGroup userGroup = (ATUserGroup) userGroupsIter.next();
			if (groupNames.contains(userGroup.getName())) {
				userGroupsIter.remove();
			}
		}
		return saveUser(user);
	}

	public LoginAttemptService getLoginAttemptService() {
		return loginAttemptService;
	}

	public void setLoginAttemptService(LoginAttemptService loginAttemptService) {
		this.loginAttemptService = loginAttemptService;
	}
	
	/**
	 * will not beused if loginAttemptService is set
	 * @param maxLoginAttempts
	 */
	public void setMaxLoginAttempts(int maxLoginAttempts) {
		this.maxLoginAttempts = maxLoginAttempts;
	}
	public int getMaxLoginAttempts() {
		if (null != this.loginAttemptService) {
			return this.loginAttemptService.getMaximumAttempts();
		}
		return maxLoginAttempts;
	}

	@Override
	public void loginFailed(String username) {
		LOGGER.info("registering login attempt");
		ATUser user = getUser(username);
		int currentTry = user.getLoginAttempts() +1;
		user.setLoginAttempts(currentTry);
		user.setLastLoginAttempt(LocalDateTime.now());
		if (currentTry > getMaxLoginAttempts()) {
			user.lockAccount();
		}
		saveUser(user);
	}
	
	@Override
	public void loginSuccess(String username) {
		ATUser user = getUser(username);
		if (user.getLastLoginAttempt() != null 
				&& user.getLastLoginAttempt().isBefore(LocalDateTime.now().minusDays(7))) {
			LOGGER.info("removing login attempts");
			user.setLoginAttempts(0);
			
		}
		user.setLastLogin(LocalDateTime.now());
		saveUser(user);
	}
	
}
