package de.chandre.admintool.security.dbuser.domain;

import java.time.LocalDateTime;
import java.util.Locale;
import java.util.Set;
import java.util.TimeZone;

import org.springframework.security.core.userdetails.UserDetails;

public interface User extends UserDetails, Entity {
	
	boolean isAccountLocked();

	boolean isAccountExpired();
	
	boolean isCredentialsExpired();
	
	boolean isNotEnabled();

	Set<ATUserGroup> getUserGroups();

	Set<ATClient> getClients();

	Set<ATClient> getActiveClients();

	LocalDateTime getLastLogin();

	LocalDateTime getLastLoginAttempt();

	int getLoginAttempts();

	LocalDateTime getPasswordLinkCreated();

	String getPasswordLinkHash();

	String getPhone();

	String getEmail();

	String getLastName();

	String getFirstName();

	LocalDateTime getCredentialsExpiredSince();

	LocalDateTime getAccountLockedSince();

	LocalDateTime getAccountExpiredSince();

	String getLocale();

	Locale getLocaleAsLocale();

	String getTimeZone();

	TimeZone getTimeZoneAsTimeZone();

}
