package de.chandre.admintool.security.dbuser.domain;

import java.time.ZonedDateTime;

import org.springframework.data.domain.Persistable;

/**
 * interface for all entities
 * 
 * @author André
 * @since 1.1.7
 *
 */
public interface Entity extends Persistable<String> {
	
	Integer getVersion();

	ZonedDateTime getCreated();

	String getCreatedBy();

	ZonedDateTime getModified();

	String getModifiedBy();

}
