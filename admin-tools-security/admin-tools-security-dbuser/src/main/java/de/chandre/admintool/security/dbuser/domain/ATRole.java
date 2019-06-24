package de.chandre.admintool.security.dbuser.domain;

import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToMany;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import de.chandre.admintool.security.dbuser.Constants;

/**
 * Role entity.<br>
 * call {@link ATRole#create()} when creating an new instance
 * @author André
 * @since 1.2.0
 */
@Entity
@Table(name="AT_ROLE")
public class ATRole extends AbstractEntity implements Role, Constants {
	private static final long serialVersionUID = 709794367592250870L;
	
	public static String PREFIX = "ROLE_";
	
	@NotNull(message = MSG_KEY_PREFIX + "role.name.NotNull")
	@Size(min = 1, message = MSG_KEY_PREFIX + "role.name.Size")
	@Column(name="NAME", nullable=false, unique=true)
	private String name;
	
	@Column(name="DISPLAY_NAME", nullable=true)
	private String displayName;
	
	@Column(name="DESCRIPTION", nullable=true)
	private String description;
	
	@ManyToMany(mappedBy="roles")
	private Set<ATUserGroup> userGroups = new HashSet<>();
	
	@Column(name="ACTIVE", nullable=false)
	private boolean active;
	
	public ATRole() {
		this(null);
	}
	
	public ATRole(String authority) {
		super();
		setName(authority);
		setActive(true);
	}
	
	public static ATRole createNew() {
		return ATRole.createNew(null);
	}
	
	public static ATRole createNew(String authority) {
		ATRole role = new ATRole(authority);
		role.create();
		return role;
	}

	@Override
	public String getAuthority() {
		return name;
	}

	@Override
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = checkForPrefix(name);
	}
	
	public static String checkForPrefix(String name) {
		if (null != name && !name.startsWith(PREFIX)) {
			return PREFIX + name;
		}
		return name;
	}
	
	@Override
	public String getDisplayName() {
		return null == displayName ? removePrefix(name) : displayName;
	}
	
	public static  String removePrefix(String name) {
		if (null != name && name.startsWith(PREFIX)) {
			return name.split("_")[1];
		}
		return name;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	@Override
	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	@Override
	public Set<ATUserGroup> getUserGroups() {
		return userGroups;
	}

	public void setUserGroups(Set<ATUserGroup> userGroups) {
		this.userGroups = userGroups;
	}

	@Override
	public boolean isActive() {
		return active;
	}

	public void setActive(boolean enabled) {
		this.active = enabled;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((name == null) ? 0 : name.hashCode());
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
		ATRole other = (ATRole) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("ATRole [name=").append(name).append(", displayName=").append(displayName)
				.append(", description=").append(description).append(", active=").append(active).append(", toString()=")
				.append(super.toString()).append("]");
		return builder.toString();
	}
	
}
