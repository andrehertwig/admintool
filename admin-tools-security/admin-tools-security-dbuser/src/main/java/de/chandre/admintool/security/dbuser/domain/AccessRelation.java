package de.chandre.admintool.security.dbuser.domain;

/**
 * 
 * @author André
 * @since 1.1.7
 *
 */
public interface AccessRelation extends Entity {
	boolean isActive();

	String getDescription();

	String getDisplayName();

	String getName();
}
