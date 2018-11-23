package de.chandre.admintool.core.sec;

/**
 * role for initialization
 * @author Andre
 * @since 1.2.0
 */
public class ATInitRoleImpl implements ATInitRole {
	
	private String name;
	private String displayName;
	private String description;
	private boolean active = false;
	
	@Override
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	@Override
	public String getDisplayName() {
		return displayName;
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
		int result = 1;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
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
		ATInitRoleImpl other = (ATInitRoleImpl) obj;
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
		builder.append("ATInitRole [name=").append(name).append(", displayName=").append(displayName)
				.append(", description=").append(description).append(", active=").append(active).append("]");
		return builder.toString();
	}

	public static class ATInitRoleBuilder {
		
		private ATInitRoleImpl initRole;
		
		public static ATInitRoleBuilder builder() {
			return new ATInitRoleBuilder();
		}
		public ATInitRoleBuilder() {
			initRole = new ATInitRoleImpl();
		}
		public ATInitRoleBuilder name(String name) {
			this.initRole.setName(name);
			return this;
		}
		public ATInitRoleBuilder displayName(String displayName) {
			this.initRole.setDisplayName(displayName);
			return this;
		}
		public ATInitRoleBuilder description(String description) {
			this.initRole.setDescription(description);
			return this;
		}
		public ATInitRoleBuilder active(boolean active) {
			this.initRole.setActive(active);
			return this;
		}
		public ATInitRoleImpl build() {
			return initRole;
		}
	}
	
}
