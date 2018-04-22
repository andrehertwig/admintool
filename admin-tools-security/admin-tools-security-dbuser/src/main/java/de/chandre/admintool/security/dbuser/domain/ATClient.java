package de.chandre.admintool.security.dbuser.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import de.chandre.admintool.security.dbuser.Constants;

/**
 * 
 * @author André
 * @since 1.1.7
 */
@Entity
@Table(name="AT_CLIENT")
public class ATClient extends AbstractEntity implements Client, Constants {
	private static final long serialVersionUID = 5009540536707462910L;

	@NotNull(message = MSG_KEY_PREFIX + "client.name.NotNull")
	@Size(min = 1, message = MSG_KEY_PREFIX + "client.name.Size")
	@Column(name="NAME", nullable=false, unique=true)
	private String name;
	
	@Column(name="DISPLAY_NAME", nullable=true)
	private String displayName;
	
	@Column(name="DESCRIPTION", nullable=true)
	private String description;
	
	@Column(name="ACTIVE", nullable=false)
	private boolean active;
	
	public ATClient() {
		super();
		setActive(true);
	}
	
	public ATClient(String name) {
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
	public boolean isActive() {
		return active;
	}

	public void setActive(boolean active) {
		this.active = active;
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
		ATClient other = (ATClient) obj;
		if (active != other.active)
			return false;
		if (description == null) {
			if (other.description != null)
				return false;
		} else if (!description.equals(other.description))
			return false;
		if (displayName == null) {
			if (other.displayName != null)
				return false;
		} else if (!displayName.equals(other.displayName))
			return false;
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
		builder.append("Client [name=").append(name).append(", displayName=").append(displayName)
				.append(", description=").append(description).append(", active=").append(active).append(", toString()=")
				.append(super.toString()).append("]");
		return builder.toString();
	}
	
}
