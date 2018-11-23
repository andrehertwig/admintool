package de.chandre.admintool.core.sec;

/**
 * role for initialization
 * @author Andre
 * @since 1.2.0
 */
public interface ATInitRole {

	public String getName();
	public String getDisplayName();
	public String getDescription();
	public boolean isActive();
	
	default String getNamePrefixed() {
		if (null != getName() && !getName().startsWith(AdminToolRoles.PREFIX)) {
			return AdminToolRoles.PREFIX + getName();
		}
		return getName();
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
		public ATInitRole build() {
			return initRole;
		}
	}
	
}
