package de.chandre.admintool.security.dbuser;

import java.time.Period;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import javax.annotation.PostConstruct;

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * 
 * @author Andre
 * @since 1.2.0
 */
@Component("adminToolSecDBProperties")
@ConfigurationProperties(prefix = AdminToolSecDBProperties.PREFIX, ignoreUnknownFields = true)
public class AdminToolSecDBProperties {
	
	public static final String PREFIX = "admintool.security.";
	
	private boolean enabled = true;
	
	private int position;
	
	private String validatorCdnPath;
	
	private String mustacheVersion = "2.3.0";
	
	private String select2Version = "4.0.5";
	
	private boolean addMissingRolesAutomatically = true;
	
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

	public boolean isAddMissingRolesAutomatically() {
		return addMissingRolesAutomatically;
	}

	public void setAddMissingRolesAutomatically(boolean addMissingRolesAutomatically) {
		this.addMissingRolesAutomatically = addMissingRolesAutomatically;
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
		
		private boolean directPasswordChangeAllowed = true;
		private boolean directPasswordChangeInProfileAllowed = true;
		
		private String passwordHashPeriod = "P7D";
		
		private String maxPasswordAge= "";
		
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
		public boolean isDirectPasswordChangeAllowed() {
			return directPasswordChangeAllowed;
		}
		public void setDirectPasswordChangeAllowed(boolean directPasswordChangeAllowed) {
			this.directPasswordChangeAllowed = directPasswordChangeAllowed;
		}
		public boolean isDirectPasswordChangeInProfileAllowed() {
			return directPasswordChangeInProfileAllowed;
		}
		public void setDirectPasswordChangeInProfileAllowed(boolean directPasswordChangeInProfileAllowed) {
			this.directPasswordChangeInProfileAllowed = directPasswordChangeInProfileAllowed;
		}
		public void setPasswordHashPeriod(String passwordHashPeriod) {
			this.passwordHashPeriod = passwordHashPeriod;
		}
		public Period getPasswordHashPeriod() {
			return Period.parse(passwordHashPeriod);
		}
		
		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder();
			builder.append("Users [availableLocales=").append(availableLocales).append(", username=").append(username)
					.append(", password=").append(password).append(", firstName=").append(firstName)
					.append(", lastName=").append(lastName).append(", email=").append(email).append(", phone=")
					.append(phone).append(", directPasswordChangeAllowed=").append(directPasswordChangeAllowed)
					.append(", directPasswordChangeInProfileAllowed=").append(directPasswordChangeInProfileAllowed)
					.append(", passwordHashPeriod=").append(passwordHashPeriod).append("]");
			return builder.toString();
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
		
		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder();
			builder.append("UserGroups [name=").append(name).append(", displayName=").append(displayName)
					.append(", description=").append(description).append("]");
			return builder.toString();
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
		
		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder();
			builder.append("Roles [name=").append(name).append(", displayName=").append(displayName)
					.append(", description=").append(description).append("]");
			return builder.toString();
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
		
		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder();
			builder.append("Clients [enabled=").append(enabled).append(", name=").append(name).append(", displayName=")
					.append(displayName).append(", description=").append(description).append("]");
			return builder.toString();
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

		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder();
			builder.append("Validations [maxLength=").append(maxLength).append(", minLength=").append(minLength)
					.append(", patternStr=").append(patternStr).append(", patternCaseSensitive=")
					.append(patternCaseSensitive).append(", pattern=").append(pattern).append("]");
			return builder.toString();
		}
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AdminToolSecDBProperties [enabled=").append(enabled).append(", position=").append(position)
				.append(", validatorCdnPath=").append(validatorCdnPath).append(", mustacheVersion=")
				.append(mustacheVersion).append(", select2Version=").append(select2Version)
				.append(", addMissingRolesAutomatically=").append(addMissingRolesAutomatically).append(", users=")
				.append(users).append(", userGroups=").append(userGroups).append(", roles=").append(roles)
				.append(", clients=").append(clients).append("]");
		return builder.toString();
	}
	
}
