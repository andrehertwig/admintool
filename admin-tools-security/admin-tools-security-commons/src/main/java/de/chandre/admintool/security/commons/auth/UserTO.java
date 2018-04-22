package de.chandre.admintool.security.commons.auth;

import java.io.Serializable;
import java.util.Set;

/**
 * Transfer object for user view
 * @author Andre
 * @since 1.1.5
 */
public class UserTO implements Serializable {
	private static final long serialVersionUID = 7636129411047455210L;
	
	private String username;
	private boolean newState;
	
	private String password;
	
	private String firstName;
	private String lastName;
	private String phone;
	private String email;
	
	private String locale;
	private String timeZone;
	
	private Set<String> authorities;
	private Set<String> clients;
	
	public String getUsername() {
		return username;
	}
	public void setUsername(String username) {
		this.username = username;
	}
	public boolean getNewState() {
		return newState;
	}
	public void setNewState(boolean newState) {
		this.newState = newState;
	}
	
	public Set<String> getAuthorities() {
		return authorities;
	}
	public void setAuthorities(Set<String> authorities) {
		this.authorities = authorities;
	}
	
	public Set<String> getClients() {
		return clients;
	}
	public void setClients(Set<String> clients) {
		this.clients = clients;
	}
	/**
	 * the password could be null in case of existing user 
	 * @return
	 */
	public String getPassword() {
		return password;
	}
	public void setPassword(String password) {
		this.password = password;
	}
	
	public String getFirstName() {
		return firstName;
	}
	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}
	public String getLastName() {
		return lastName;
	}
	public void setLastName(String lastName) {
		this.lastName = lastName;
	}
	public String getPhone() {
		return phone;
	}
	public void setPhone(String phone) {
		this.phone = phone;
	}
	public String getEmail() {
		return email;
	}
	public void setEmail(String email) {
		this.email = email;
	}
	public String getLocale() {
		return locale;
	}
	public void setLocale(String locale) {
		this.locale = locale;
	}
	public String getTimeZone() {
		return timeZone;
	}
	public void setTimeZone(String timeZone) {
		this.timeZone = timeZone;
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((authorities == null) ? 0 : authorities.hashCode());
		result = prime * result + (newState ? 1231 : 1237);
		result = prime * result + ((password == null) ? 0 : password.hashCode());
		result = prime * result + ((username == null) ? 0 : username.hashCode());
		return result;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		UserTO other = (UserTO) obj;
		if (authorities == null) {
			if (other.authorities != null)
				return false;
		} else if (!authorities.equals(other.authorities))
			return false;
		if (newState != other.newState)
			return false;
		if (password == null) {
			if (other.password != null)
				return false;
		} else if (!password.equals(other.password))
			return false;
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
		builder.append("UserTO [username=").append(username).append(", newState=").append(newState)
				.append(", password=").append(null != password ? ("***(" + password.length() + ")") : null)
				.append(", firstName=").append(firstName).append(", lastName=")
				.append(lastName).append(", phone=").append(phone).append(", email=").append(email)
				.append(", authorities=").append(authorities).append("]");
		return builder.toString();
	}
}
