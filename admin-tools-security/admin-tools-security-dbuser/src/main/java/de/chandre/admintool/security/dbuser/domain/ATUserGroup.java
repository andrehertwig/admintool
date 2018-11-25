package de.chandre.admintool.security.dbuser.domain;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
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

import de.chandre.admintool.security.dbuser.Constants;

/**
 * 
 * @author André
 * @since 1.2.0
 */
@Entity
@Table(name="AT_USER_GROUP")
public class ATUserGroup extends AbstractEntity implements UserGroup, Constants {
	private static final long serialVersionUID = 228361760358241987L;
	
	@NotNull(message = MSG_KEY_PREFIX + "userGroup.name.NotNull")
	@Size(min = 1, message = MSG_KEY_PREFIX + "userGroup.name.Size")
	@Column(name="NAME", nullable=false, unique=true)
	private String name;
	
	@Column(name="DISPLAY_NAME", nullable=true)
	private String displayName;
	
	@Column(name="DESCRIPTION", nullable=true)
	private String description;
	
	@Valid
	@NotNull(message = MSG_KEY_PREFIX + "userGroup.roles.NotNull")
	@Size(min = 1, message = MSG_KEY_PREFIX + "userGroup.roles.Size")
	@ManyToMany(fetch=FetchType.EAGER)
	@JoinTable(name="AT_USERGROUPS_ROLES",
		joinColumns=@JoinColumn(name="USERGROUP_UUID", referencedColumnName="UUID"),
		inverseJoinColumns=@JoinColumn(name="ROLE_UUID", referencedColumnName="UUID"))
	private Set<ATRole> roles = new HashSet<>();
	
	@ManyToMany(mappedBy="userGroups")
	private Set<ATUser> users = new HashSet<>();
	
	@Column(name="ACTIVE", nullable=false)
	private boolean active;
	
	public ATUserGroup() {
		super();
		setActive(true);
	}
	
	public ATUserGroup(String name) {
		super();
		setActive(true);
		setName(name);
	}

	@Override
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@Override
	public String getDisplayName() {
		return null == displayName ? name : displayName;
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
	public Set<ATRole> getRoles() {
		return roles;
	}
	
	@Override
	public Stream<ATRole> getActiveRoles() {
		if (null != roles) {
			return roles.stream().filter(ATRole::isActive);
		}
		return Stream.empty();
	}
	
	public Set<String> getRoleNames() {
		if (null != roles) {
			return roles.stream().map(role -> role.getName()).collect(Collectors.toSet());
		}
		return Collections.emptySet();
	}
	
	public List<String> getRoleDisplayNames() {
		if (null != roles) {
			return roles.stream().map(role -> role.getDisplayName()).collect(Collectors.toList());
		}
		return Collections.emptyList();
	}

	public void setRoles(Set<ATRole> roles) {
		this.roles = roles;
	}
	
	public void setRoles(List<ATRole> roles) {
		this.roles.clear();
		if (null != roles) {
			this.roles = new HashSet<>(roles.size());
			this.roles.addAll(roles);
		}
	}

	@Override
	public Set<ATUser> getUsers() {
		return users;
	}

	public void setUsers(Set<ATUser> users) {
		this.users = users;
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
		ATUserGroup other = (ATUserGroup) obj;
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
		builder.append("ATUserGroup [name=").append(name).append(", displayName=").append(displayName)
				.append(", description=").append(description).append(", roles=").append(roles)
				.append(", active=").append(active).append(", toString()=").append(super.toString())
				.append("]");
		return builder.toString();
	}
	
}
