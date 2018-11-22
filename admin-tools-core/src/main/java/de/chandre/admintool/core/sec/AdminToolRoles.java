package de.chandre.admintool.core.sec;

import java.util.Collection;

/**
 * interface for roles of a admintool component
 * (placed in core to avoid interdependencies)
 * @author Andre
 * @since 1.2.0
 */
public interface AdminToolRoles {
	
	public static String PREFIX = "ROLE_";
	
	Collection<String> getRoles();
}
