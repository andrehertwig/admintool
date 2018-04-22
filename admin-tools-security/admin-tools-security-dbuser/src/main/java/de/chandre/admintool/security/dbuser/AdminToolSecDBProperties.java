package de.chandre.admintool.security.dbuser;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import javax.annotation.PostConstruct;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;


@Component
@ConfigurationProperties(prefix = AdminToolSecDBProperties.PREFIX, ignoreUnknownFields = true)
public class AdminToolSecDBProperties {
	
	public static final String PREFIX = "admintool.security.";
	
	private boolean enabled = true;
	
	private int position;
	
	private String validatorCdnPath;
	
	private String mustacheVersion = "2.3.0";
	
	private String select2Version = "4.0.5";
	
	@Value("#{'${admintool.security.ui.securityRoles.groups:}'.split(';')}")
	private Set<String> securityRolesGroups = new HashSet<>();
	
	@Value("#{'${admintool.security.ui.securityRoles.roles:}'.split(';')}")
	private Set<String> securityRolesRoles = new HashSet<>();
	
	@Value("#{'${admintool.security.ui.securityRoles.clients:}'.split(';')}")
	private Set<String> securityRolesClients = new HashSet<>();
	
	@Value("#{'${admintool.security.ui.securityRoles.users:}'.split(';')}")
	private Set<String> securityRolesUsers = new HashSet<>();
	
	private Users users = new Users();
	
	private UserGroups userGroups = new UserGroups();
	
	private Roles roles = new Roles();
	
	private Clients clients = new Clients();
	
	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	public String getValidatorCdnPath() {
		return validatorCdnPath;
	}

	public void setValidatorCdnPath(String validatorCdnPath) {
		this.validatorCdnPath = validatorCdnPath;
	}

	public String getMustacheVersion() {
		return mustacheVersion;
	}

	public void setMustacheVersion(String mustacheVersion) {
		this.mustacheVersion = mustacheVersion;
	}
	
	public String getSelect2Version() {
		return select2Version;
	}

	public void setSelect2Version(String select2Version) {
		this.select2Version = select2Version;
	}

	public Set<String> getSecurityRolesGroups() {
		return securityRolesGroups;
	}

	public void setSecurityRolesGroups(Set<String> securityRolesGroups) {
		this.securityRolesGroups = securityRolesGroups;
	}

	public Set<String> getSecurityRolesRoles() {
		return securityRolesRoles;
	}

	public void setSecurityRolesRoles(Set<String> securityRolesRoles) {
		this.securityRolesRoles = securityRolesRoles;
	}

	public Set<String> getSecurityRolesClients() {
		return securityRolesClients;
	}

	public void setSecurityRolesClients(Set<String> securityRolesClients) {
		this.securityRolesClients = securityRolesClients;
	}

	public Set<String> getSecurityRolesUsers() {
		return securityRolesUsers;
	}

	public void setSecurityRolesUsers(Set<String> securityRolesUsers) {
		this.securityRolesUsers = securityRolesUsers;
	}

	public Users getUsers() {
		return users;
	}

	public void setUsers(Users users) {
		this.users = users;
	}

	public UserGroups getUserGroups() {
		return userGroups;
	}

	public void setUserGroups(UserGroups userGroups) {
		this.userGroups = userGroups;
	}

	public Roles getRoles() {
		return roles;
	}

	public void setRoles(Roles roles) {
		this.roles = roles;
	}
	
	public Clients getClients() {
		return clients;
	}

	public void setClients(Clients clients) {
		this.clients = clients;
	}
	
	public static class Users {
		
		private List<String> availableLocales = new ArrayList<>();
		
		private Validations username = new Validations();
		private Validations password = new Validations();
		private Validations firstName = new Validations();
		private Validations lastName = new Validations();
		private Validations email = new Validations();
		private Validations phone = new Validations();
		
		
		public List<String> getAvailableLocales() {
			return availableLocales;
		}
		public void setAvailableLocales(List<String> availableLocales) {
			this.availableLocales = availableLocales;
		}
		public Validations getUsername() {
			return username;
		}
		public void setUsername(Validations username) {
			this.username = username;
		}
		public Validations getPassword() {
			return password;
		}
		public void setPassword(Validations password) {
			this.password = password;
		}
		public Validations getFirstName() {
			return firstName;
		}
		public void setFirstName(Validations firstName) {
			this.firstName = firstName;
		}
		public Validations getLastName() {
			return lastName;
		}
		public void setLastName(Validations lastName) {
			this.lastName = lastName;
		}
		public Validations getEmail() {
			return email;
		}
		public void setEmail(Validations email) {
			this.email = email;
		}
		public Validations getPhone() {
			return phone;
		}
		public void setPhone(Validations phone) {
			this.phone = phone;
		}
	}
	
	public static class UserGroups {
		private Validations name = new Validations();
		private Validations displayName = new Validations();
		private Validations description = new Validations();
		
		public Validations getName() {
			return name;
		}
		public void setName(Validations name) {
			this.name = name;
		}
		public Validations getDisplayName() {
			return displayName;
		}
		public void setDisplayName(Validations displayName) {
			this.displayName = displayName;
		}
		public Validations getDescription() {
			return description;
		}
		public void setDescription(Validations description) {
			this.description = description;
		}
	}

	public static class Roles {
		private Validations name = new Validations();
		private Validations displayName = new Validations();
		private Validations description = new Validations();
		
		public Validations getName() {
			return name;
		}
		public void setName(Validations name) {
			this.name = name;
		}
		public Validations getDisplayName() {
			return displayName;
		}
		public void setDisplayName(Validations displayName) {
			this.displayName = displayName;
		}
		public Validations getDescription() {
			return description;
		}
		public void setDescription(Validations description) {
			this.description = description;
		}
	}
	
	public static class Clients {
		private boolean enabled = true;
		private Validations name = new Validations();
		private Validations displayName = new Validations();
		private Validations description = new Validations();
		
		public boolean isEnabled() {
			return enabled;
		}
		public void setEnabled(boolean enabled) {
			this.enabled = enabled;
		}
		public Validations getName() {
			return name;
		}
		public void setName(Validations name) {
			this.name = name;
		}
		public Validations getDisplayName() {
			return displayName;
		}
		public void setDisplayName(Validations displayName) {
			this.displayName = displayName;
		}
		public Validations getDescription() {
			return description;
		}
		public void setDescription(Validations description) {
			this.description = description;
		}
	}
	
	public static class Validations {
		private int maxLength;
		private int minLength;
		private String patternStr;
		private boolean patternCaseSensitive=true;
		
		private Pattern pattern;
		
		@PostConstruct
		private void init() {
			if(minLength > 0 && minLength > maxLength) {
				throw new IllegalArgumentException("The minLength of validation config should be smaller than or equals to maxLegth!");
			}
			if (StringUtils.isNotBlank(patternStr)) {
				pattern = Pattern.compile(patternStr, patternCaseSensitive ? 0 : Pattern.CASE_INSENSITIVE);
			}
		}
		
		public int getMaxLength() {
			return maxLength;
		}
		public void setMaxLength(int maxLength) {
			this.maxLength = maxLength;
		}
		public int getMinLength() {
			return minLength;
		}
		public void setMinLength(int minLength) {
			this.minLength = minLength;
		}
		public String getPatternStr() {
			return patternStr;
		}
		public void setPatternStr(String patternStr) {
			this.patternStr = patternStr;
		}
		public boolean isPatternCaseSensitive() {
			return patternCaseSensitive;
		}
		public void setPatternCaseSensitive(boolean patternCaseSensitive) {
			this.patternCaseSensitive = patternCaseSensitive;
		}
		public Pattern getPattern() {
			return pattern;
		}
	}
	
}
