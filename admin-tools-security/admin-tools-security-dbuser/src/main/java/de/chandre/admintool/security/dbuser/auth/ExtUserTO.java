package de.chandre.admintool.security.dbuser.auth;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Set;

import de.chandre.admintool.security.commons.auth.UserTO;

/**
 * 
 * @author André
 * @since 1.2.0
 *
 */
public class ExtUserTO extends UserTO {
	private static final long serialVersionUID = 8288644849241619248L;
	
	private ZonedDateTime passwordDate;
	
	private Set<String> activeRoles;
	
	private boolean accountExpired;
	private boolean accountLocked;
	private boolean credentialsExpired;
	
	private ZonedDateTime accountExpiredSince;
	private ZonedDateTime accountLockedSince;
	private ZonedDateTime credentialsExpiredSince;
	
	private int loginAttempts;
	private ZonedDateTime lastLoginAttempt;
	
	private ZonedDateTime passwordLinkCreated;
	
	private ZonedDateTime lastLogin;
	
	private ZonedDateTime created;
	private String createdBy;
	
	private ZonedDateTime modified;
	private String modifiedBy;
	
	private String passwordDateISO;
	private String accountExpiredSinceISO;
	private String accountLockedSinceISO;
	private String credentialsExpiredSinceISO;
	private String lastLoginAttemptISO;
	private String passwordLinkCreatedISO;
	private String lastLoginISO;
	private String createdISO;
	private String modifiedISO;

	public ZonedDateTime getPasswordDate() {
		return passwordDate;
	}
	
	private static String getISODate(ZonedDateTime dateTime) {
		if (null != dateTime) {
			return dateTime.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
		}
		return null;
	}

	public void setPasswordDate(ZonedDateTime passwordDate) {
		this.passwordDate = passwordDate;
		this.passwordDateISO = getISODate(passwordDate);
	}

	public Set<String> getActiveRoles() {
		return activeRoles;
	}

	public void setActiveRoles(Set<String> activeRoles) {
		this.activeRoles = activeRoles;
	}
	
	public boolean isAccountExpired() {
		return accountExpired;
	}

	public void setAccountExpired(boolean accountExpired) {
		this.accountExpired = accountExpired;
	}

	public boolean isAccountLocked() {
		return accountLocked;
	}

	public void setAccountLocked(boolean accountLocked) {
		this.accountLocked = accountLocked;
	}

	public boolean isCredentialsExpired() {
		return credentialsExpired;
	}

	public void setCredentialsExpired(boolean credentialsExpired) {
		this.credentialsExpired = credentialsExpired;
	}

	public ZonedDateTime getAccountExpiredSince() {
		return accountExpiredSince;
	}

	public void setAccountExpiredSince(ZonedDateTime accountExpiredSince) {
		this.accountExpiredSince = accountExpiredSince;
		this.accountExpiredSinceISO = getISODate(accountExpiredSince);
	}

	public ZonedDateTime getAccountLockedSince() {
		return accountLockedSince;
	}

	public void setAccountLockedSince(ZonedDateTime accountLockedSince) {
		this.accountLockedSince = accountLockedSince;
		this.accountLockedSinceISO = getISODate(accountLockedSince);
	}

	public ZonedDateTime getCredentialsExpiredSince() {
		return credentialsExpiredSince;
	}

	public void setCredentialsExpiredSince(ZonedDateTime credentialsExpiredSince) {
		this.credentialsExpiredSince = credentialsExpiredSince;
		this.credentialsExpiredSinceISO = getISODate(credentialsExpiredSince);
	}

	public int getLoginAttempts() {
		return loginAttempts;
	}

	public void setLoginAttempts(int loginAttempts) {
		this.loginAttempts = loginAttempts;
	}

	public ZonedDateTime getLastLoginAttempt() {
		return lastLoginAttempt;
	}

	public void setLastLoginAttempt(ZonedDateTime lastLoginAttempt) {
		this.lastLoginAttempt = lastLoginAttempt;
		this.lastLoginAttemptISO = getISODate(lastLoginAttempt);
	}

	public ZonedDateTime getPasswordLinkCreated() {
		return passwordLinkCreated;
	}

	public void setPasswordLinkCreated(ZonedDateTime passwordLinkCreated) {
		this.passwordLinkCreated = passwordLinkCreated;
		this.passwordLinkCreatedISO = getISODate(passwordLinkCreated);
	}

	public ZonedDateTime getLastLogin() {
		return lastLogin;
	}

	public void setLastLogin(ZonedDateTime lastLogin) {
		this.lastLogin = lastLogin;
		this.lastLoginISO = getISODate(lastLogin);
	}

	public ZonedDateTime getCreated() {
		return created;
	}

	public void setCreated(ZonedDateTime created) {
		this.created = created;
		this.createdISO = getISODate(created);
	}

	public String getCreatedBy() {
		return createdBy;
	}

	public void setCreatedBy(String createdBy) {
		this.createdBy = createdBy;
	}

	public ZonedDateTime getModified() {
		return modified;
	}

	public void setModified(ZonedDateTime modified) {
		this.modified = modified;
		this.modifiedISO = getISODate(modified);
	}

	public String getModifiedBy() {
		return modifiedBy;
	}

	public void setModifiedBy(String modifiedBy) {
		this.modifiedBy = modifiedBy;
	}

	public String getPasswordDateISO() {
		return passwordDateISO;
	}

	public String getAccountExpiredSinceISO() {
		return accountExpiredSinceISO;
	}

	public String getAccountLockedSinceISO() {
		return accountLockedSinceISO;
	}

	public String getCredentialsExpiredSinceISO() {
		return credentialsExpiredSinceISO;
	}

	public String getLastLoginAttemptISO() {
		return lastLoginAttemptISO;
	}

	public String getPasswordLinkCreatedISO() {
		return passwordLinkCreatedISO;
	}

	public String getLastLoginISO() {
		return lastLoginISO;
	}

	public String getCreatedISO() {
		return createdISO;
	}

	public String getModifiedISO() {
		return modifiedISO;
	}
}
