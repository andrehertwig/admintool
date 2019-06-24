package de.chandre.admintool.security.dbuser.domain;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TimeZone;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.Table;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import org.apache.commons.lang3.LocaleUtils;
import org.springframework.security.core.GrantedAuthority;

import de.chandre.admintool.security.dbuser.ATSecDBUtils;
import de.chandre.admintool.security.dbuser.Constants;

/**
 * 
 * @author André
 * @since 1.2.0
 */
@Entity
@Table(name="AT_USER")
public class ATUser extends AbstractEntity implements User, Constants {
	private static final long serialVersionUID = 5937447636484913178L;

	@NotNull(message = MSG_KEY_PREFIX + "user.username.NotNull")
	@Size(min = 1, message = MSG_KEY_PREFIX + "user.username.Size")
	@Column(name="USERNAME", nullable=false, unique=true)
	private String username;
	
	@Column(name="PASSWORD", nullable=false)
	private String password;
	
	@Size(min = 0, max=5, message = MSG_KEY_PREFIX + "user.locale.Size")
	@Column(name="LOCALE", nullable=false)
	private String locale;
	
	@Column(name="TIMEZONE", nullable=false)
	private String timeZone;
	
	@Column(name="PASSWORD_DATE", nullable=true)
	private ZonedDateTime passwordDate;
	
	@Valid
	@NotNull(message = MSG_KEY_PREFIX + "user.userGroups.NotNull")
	@Size(min = 1, message = MSG_KEY_PREFIX + "user.userGroups.Size")
	@ManyToMany(fetch=FetchType.EAGER)
	@JoinTable(name="AT_USER_USERGROUPS",
		joinColumns=@JoinColumn(name="USER_UUID", referencedColumnName="UUID"),
		inverseJoinColumns=@JoinColumn(name="USERGROUP_UUID", referencedColumnName="UUID"))
	private Set<ATUserGroup> userGroups = new HashSet<>();
	
	@Valid
	@Size(min = 0)
	@ManyToMany(fetch=FetchType.EAGER)
	@JoinTable(name="AT_USER_CLIENTS",
		joinColumns=@JoinColumn(name="USER_UUID", referencedColumnName="UUID"),
		inverseJoinColumns=@JoinColumn(name="CLIENT_UUID", referencedColumnName="UUID"))
	private Set<ATClient> clients = new HashSet<>();
	
	@Column(name="ACC_NON_EXP", nullable=false)
	private boolean accountNonExpired;
	@Column(name="ACC_NON_LOCK", nullable=false)
	private boolean accountNonLocked;
	@Column(name="CRED_NON_EXP", nullable=false)
	private boolean credentialsNonExpired;
	
	@Column(name="ACC_EXP_SINCE", nullable=true)
	private ZonedDateTime accountExpiredSince;
	@Column(name="ACC_LOCK_SINCE", nullable=true)
	private ZonedDateTime accountLockedSince;
	@Column(name="CRED_EXP_SINCE", nullable=true)
	private ZonedDateTime credentialsExpiredSince;
	
	@Column(name="FIRSTNAME", nullable=true)
	private String firstName;
	@Column(name="LASTNAME", nullable=true)
	private String lastName;
	@Column(name="EMAIL", nullable=true)
	private String email;
	@Column(name="PHONE", nullable=true)
	private String phone;
	
	@Column(name="PWD_LINK_HASH", nullable=true)
	private String passwordLinkHash;
	@Column(name="PWD_LINK_CREATED", nullable=true)
	private ZonedDateTime passwordLinkCreated;
	
	@Column(name="LOGIN_ATTEMPTS", nullable=false)
	private int loginAttempts;
	@Column(name="LAST_LOGIN_ATTEMPT", nullable=true)
	private ZonedDateTime lastLoginAttempt;
	
	@Column(name="LAST_LOGIN", nullable=true)
	private ZonedDateTime lastLogin;
	
	public ATUser() {
		super();
		init();
	}
	
	public ATUser(String username, String password) {
		super();
		init();
		setUsername(username);
		setPassword(password);
	}
	
	public static ATUser createNew() {
		ATUser user = new ATUser();
		user.create();
		return user;
	}
	
	public static ATUser createNew(String username, String password) {
		ATUser user = new ATUser(username, password);
		user.create();
		return user;
	}
	
	private void init() {
		this.accountNonExpired = true;
		this.accountNonLocked = true;
		this.credentialsNonExpired = true;
		this.loginAttempts=0;
	}
	
	private Stream<? extends ATRole> getRoles() {
		return this.userGroups.stream()
			//only active groups
			.filter(ATUserGroup::isActive)
			//merge active roles
			.flatMap(ATUserGroup::getActiveRoles);
	}

	@Override
	public Collection<? extends GrantedAuthority> getAuthorities() {
		if (this.userGroups != null) {
			return getRoles().collect(Collectors.toSet());
		}
		return Collections.emptySet();
	}
	
	public Set<String> getActiveAuthorityNames() {
		if (this.userGroups != null) {
			return getRoles().map(ATRole::getName).collect(Collectors.toSet());
		}
		return Collections.emptySet();
	}
	
	public List<String> getActiveAuthorityDisplayNames() {
		if (this.userGroups != null) {
			return getRoles().map(ATRole::getDisplayName).collect(Collectors.toList());
		}
		return Collections.emptyList();
	}

	@Override
	public String getPassword() {
		return this.password;
	}

	public ZonedDateTime getPasswordDate() {
		return passwordDate;
	}

	@Override
	public String getUsername() {
		return this.username;
	}

	@Override
	public boolean isAccountNonExpired() {
		return this.accountNonExpired;
	}
	
	@Override
	public boolean isAccountExpired() {
		return !this.accountNonExpired;
	}

	@Override
	public boolean isAccountNonLocked() {
		return this.accountNonLocked;
	}
	
	@Override
	public boolean isAccountLocked() {
		return !this.accountNonLocked;
	}

	@Override
	public boolean isCredentialsNonExpired() {
		return this.credentialsNonExpired;
	}
	
	@Override
	public boolean isCredentialsExpired() {
		return !this.credentialsNonExpired;
	}
	
	@Override
	public boolean isNotEnabled() {
		return isAccountExpired() || isAccountLocked() || isCredentialsExpired();
	}

	@Override
	public boolean isEnabled() {
		return !isNotEnabled();
	}
	
	public void setEnabled(boolean enabled) {
		setAccountNonExpired(enabled);
		setAccountNonLocked(enabled);
		setCredentialsNonExpired(enabled);
		
		ZonedDateTime now = ZonedDateTime.now();
		setAccountExpiredSince(enabled ? null : now);
		setAccountLockedSince(enabled ? null : now);
		setCredentialsExpiredSince(enabled ? null : now);
	}
	
	@Override
	public String getLocale() {
		return this.locale;
	}
	
	@Override
	public Locale getLocaleAsLocale() {
		return null != this.locale ? LocaleUtils.toLocale(this.locale) : null;
	}
	
	public void setLocale(Locale locale) {
		if (null != locale) {
			this.locale = locale.toString();
		}
	}

	public void setLocale(String locale) {
		this.locale = locale;
	}

	@Override
	public String getTimeZone() {
		return timeZone;
	}
	
	@Override
	public TimeZone getTimeZoneAsTimeZone() {
		return TimeZone.getTimeZone(timeZone);
	}

	public void setTimeZone(String timeZone) {
		this.timeZone = timeZone;
	}
	
	public void setTimeZone(TimeZone timeZone) {
		if (null != timeZone) {
			this.timeZone = timeZone.getID();
		}
	}
	
	public void setTimeZone(ZoneId zoneId) {
		setTimeZone(TimeZone.getTimeZone(zoneId));
	}

	@Override
	public Set<ATUserGroup> getUserGroups() {
		return userGroups;
	}

	public Set<String> getUserGroupNames() {
		if (null != userGroups) {
			return userGroups.stream().map(ATUserGroup::getName).collect(Collectors.toSet());
		}
		return Collections.emptySet();
	}
	
	public List<String> getUserGroupDisplayNames() {
		if (null != userGroups) {
			return userGroups.stream().map(ATUserGroup::getDisplayName).collect(Collectors.toList());
		}
		return Collections.emptyList();
	}
	
	public void setUserGroups(Set<ATUserGroup> userGroups) {
		this.userGroups = userGroups;
	}
	
	public void setUserGroups(List<ATUserGroup> userGroups) {
		this.userGroups.clear();
		if (null != userGroups) {
			this.userGroups = new HashSet<>(userGroups.size());
			this.userGroups.addAll(userGroups);
		}
	}

	@Override
	public ZonedDateTime getAccountExpiredSince() {
		return accountExpiredSince;
	}

	public void setAccountExpiredSince(ZonedDateTime accountExpiredSince) {
		this.accountExpiredSince = accountExpiredSince;
	}

	@Override
	public ZonedDateTime getAccountLockedSince() {
		return accountLockedSince;
	}

	public void setAccountLockedSince(ZonedDateTime accountLockedSince) {
		this.accountLockedSince = accountLockedSince;
	}

	@Override
	public ZonedDateTime getCredentialsExpiredSince() {
		return credentialsExpiredSince;
	}

	public void setCredentialsExpiredSince(ZonedDateTime credentialsExpiredSince) {
		this.credentialsExpiredSince = credentialsExpiredSince;
	}

	@Override
	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	@Override
	public String getLastName() {
		return lastName;
	}

	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	@Override
	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	@Override
	public String getPhone() {
		return phone;
	}

	public void setPhone(String phone) {
		this.phone = phone;
	}

	@Override
	public String getPasswordLinkHash() {
		return passwordLinkHash;
	}

	public void setPasswordLinkHash(String passwordLinkHash) {
		if (null != passwordLinkHash) {
			this.passwordLinkHash = passwordLinkHash;
			this.passwordLinkCreated = ZonedDateTime.now();
		} else {
			this.passwordLinkHash = null;
			this.passwordLinkCreated = null;
		}
	}

	@Override
	public ZonedDateTime getPasswordLinkCreated() {
		return passwordLinkCreated;
	}

	public void setPasswordLinkCreated(ZonedDateTime passwordLinkCreated) {
		this.passwordLinkCreated = passwordLinkCreated;
	}

	@Override
	public int getLoginAttempts() {
		return loginAttempts;
	}

	public void setLoginAttempts(int loginAttempts) {
		this.loginAttempts = loginAttempts;
	}

	@Override
	public ZonedDateTime getLastLoginAttempt() {
		return lastLoginAttempt;
	}

	public void setLastLoginAttempt(ZonedDateTime lastLoginAttempt) {
		this.lastLoginAttempt = lastLoginAttempt;
	}
	
	public void setLastLoginAttemptNow() {
		this.lastLoginAttempt = ATSecDBUtils.getNowZoned(this);
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public void setPassword(String password) {
		this.password = password;
		this.passwordDate = ATSecDBUtils.getNowZoned(this);
	}

	public void setAccountNonExpired(boolean accountNonExpired) {
		this.accountNonExpired = accountNonExpired;
	}
	
	public void expireAccount() {
		this.accountNonExpired = false;
		this.accountExpiredSince = ATSecDBUtils.getNowZoned(this);
	}

	public void setAccountNonLocked(boolean accountNonLocked) {
		this.accountNonLocked = accountNonLocked;
	}
	
	public void lockAccount() {
		this.accountNonLocked = false;
		this.accountLockedSince = ATSecDBUtils.getNowZoned(this);
	}

	public void setCredentialsNonExpired(boolean credentialsNonExpired) {
		this.credentialsNonExpired = credentialsNonExpired;
	}
	
	public void expireCredentials() {
		this.credentialsNonExpired = false;
		this.credentialsExpiredSince = ATSecDBUtils.getNowZoned(this);
	}
	
	@Override
	public Set<ATClient> getClients() {
		return clients;
	}
	
	@Override
	public Set<ATClient> getActiveClients() {
		if (this.clients != null) {
			return this.clients.stream().filter(ATClient::isActive).collect(Collectors.toSet());
		}
		return Collections.emptySet();
	}
	
	public Set<String> getClientNames() {
		if (null != clients) {
			return clients.stream().map(client -> client.getName()).collect(Collectors.toSet());
		}
		return Collections.emptySet();
	}
	
	public List<String> getClientDisplayNames() {
		if (null != clients) {
			return clients.stream().map(client -> client.getDisplayName()).collect(Collectors.toList());
		}
		return Collections.emptyList();
	}

	public void setClients(Set<ATClient> clients) {
		this.clients = clients;
	}
	
	public void setClients(List<ATClient> clients) {
		this.clients.clear();
		if (null != clients) {
			this.clients = new HashSet<>(clients.size());
			this.clients.addAll(clients);
		}
	}

	@Override
	public ZonedDateTime getLastLogin() {
		return lastLogin;
	}

	public void setLastLogin(ZonedDateTime lastLogin) {
		this.lastLogin = lastLogin;
	}
	
	public void setLastLoginNow() {
		this.lastLogin = ATSecDBUtils.getNowZoned(this);
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((username == null) ? 0 : username.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		ATUser other = (ATUser) obj;
		if (username == null) {
			if (other.username != null)
				return false;
		} else if (!username.equals(other.username))
			return false;
		return true;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("User [username=").append(username).append(", password=****")
				.append(", userGroups=").append(userGroups)
				.append(", clients=").append(clients)
				.append(", accountNonExpired=").append(accountNonExpired)
				.append(", accountNonLocked=").append(accountNonLocked).append(", credentialsNonExpired=")
				.append(credentialsNonExpired).append(", accountExpiredSince=")
				.append(accountExpiredSince).append(", accountLockedSince=").append(accountLockedSince)
				.append(", credentialsExpiredSince=").append(credentialsExpiredSince).append(", firstName=")
				.append(firstName).append(", lastName=").append(lastName).append(", email=").append(email)
				.append(", phone=").append(phone).append(", passwordLinkHash=").append(passwordLinkHash)
				.append(", passwordLinkCreated=").append(passwordLinkCreated).append(", loginAttempts=")
				.append(loginAttempts).append(", lastLoginAttempt=").append(lastLoginAttempt)
				.append(", lastLogin=").append(lastLogin).append(", toString()=")
				.append(super.toString()).append("]");
		return builder.toString();
	}
}
