package de.chandre.admintool.security.dbuser.auth;

import java.io.Serializable;
import java.util.Set;

/**
 * 
 * @author André
 * @since 1.2.0
 *
 */
public class AccessRelationTO implements Serializable {
	private static final long serialVersionUID = -5482199588386327064L;
	
	private String name;
	private String displayName;
	private String description;
	private boolean active;
	
	private Set<String> relationNames;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public boolean isActive() {
		return active;
	}

	public void setActive(boolean active) {
		this.active = active;
	}

	public Set<String> getRelationNames() {
		return relationNames;
	}

	public void setRelationNames(Set<String> relationNames) {
		this.relationNames = relationNames;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AccessRelationTO [name=").append(name).append(", displayName=").append(displayName)
				.append(", description=").append(description).append(", active=").append(active)
				.append(", relationNames=").append(relationNames).append("]");
		return builder.toString();
	}
}
