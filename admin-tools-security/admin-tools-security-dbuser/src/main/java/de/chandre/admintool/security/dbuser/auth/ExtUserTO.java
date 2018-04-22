package de.chandre.admintool.security.dbuser.auth;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Set;

import de.chandre.admintool.security.commons.auth.UserTO;

/**
 * 
 * @author André
 * @since 1.1.7
 *
 */
public class ExtUserTO extends UserTO {
	private static final long serialVersionUID = 8288644849241619248L;
	
	private LocalDateTime passwordDate;
	
	private Set<String> activeRoles;
	
	private boolean accountExpired;
	private boolean accountLocked;
	private boolean credentialsExpired;
	
	private LocalDateTime accountExpiredSince;
	private LocalDateTime accountLockedSince;
	private LocalDateTime credentialsExpiredSince;
	
	private int loginAttempts;
	private LocalDateTime lastLoginAttempt;
	
	private LocalDateTime passwordLinkCreated;
	
	private LocalDateTime lastLogin;
	
	private LocalDateTime created;
	private String createdBy;
	
	private LocalDateTime modified;
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

	public LocalDateTime getPasswordDate() {
		return passwordDate;
	}
	
	private static String getISODate(LocalDateTime dateTime) {
		if (null != dateTime) {
			return dateTime.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
		}
		return null;
	}

	public void setPasswordDate(LocalDateTime passwordDate) {
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

	public LocalDateTime getAccountExpiredSince() {
		return accountExpiredSince;
	}

	public void setAccountExpiredSince(LocalDateTime accountExpiredSince) {
		this.accountExpiredSince = accountExpiredSince;
		this.accountExpiredSinceISO = getISODate(accountExpiredSince);
	}

	public LocalDateTime getAccountLockedSince() {
		return accountLockedSince;
	}

	public void setAccountLockedSince(LocalDateTime accountLockedSince) {
		this.accountLockedSince = accountLockedSince;
		this.accountLockedSinceISO = getISODate(accountLockedSince);
	}

	public LocalDateTime getCredentialsExpiredSince() {
		return credentialsExpiredSince;
	}

	public void setCredentialsExpiredSince(LocalDateTime credentialsExpiredSince) {
		this.credentialsExpiredSince = credentialsExpiredSince;
		this.credentialsExpiredSinceISO = getISODate(credentialsExpiredSince);
	}

	public int getLoginAttempts() {
		return loginAttempts;
	}

	public void setLoginAttempts(int loginAttempts) {
		this.loginAttempts = loginAttempts;
	}

	public LocalDateTime getLastLoginAttempt() {
		return lastLoginAttempt;
	}

	public void setLastLoginAttempt(LocalDateTime lastLoginAttempt) {
		this.lastLoginAttempt = lastLoginAttempt;
		this.lastLoginAttemptISO = getISODate(lastLoginAttempt);
	}

	public LocalDateTime getPasswordLinkCreated() {
		return passwordLinkCreated;
	}

	public void setPasswordLinkCreated(LocalDateTime passwordLinkCreated) {
		this.passwordLinkCreated = passwordLinkCreated;
		this.passwordLinkCreatedISO = getISODate(passwordLinkCreated);
	}

	public LocalDateTime getLastLogin() {
		return lastLogin;
	}

	public void setLastLogin(LocalDateTime lastLogin) {
		this.lastLogin = lastLogin;
		this.lastLoginISO = getISODate(lastLogin);
	}

	public LocalDateTime getCreated() {
		return created;
	}

	public void setCreated(LocalDateTime created) {
		this.created = created;
		this.createdISO = getISODate(created);
	}

	public String getCreatedBy() {
		return createdBy;
	}

	public void setCreatedBy(String createdBy) {
		this.createdBy = createdBy;
	}

	public LocalDateTime getModified() {
		return modified;
	}

	public void setModified(LocalDateTime modified) {
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
