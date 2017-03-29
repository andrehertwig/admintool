package de.chandre.admintool.core.security.auth;

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
	private Set<String> authorities;
	
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
				.append(", authorities=").append(authorities).append(", password=")
				.append(null != password ? ("***(" + password.length() + ")") : null).append("]");
		return builder.toString();
	}
}
