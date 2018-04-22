package de.chandre.admintool.security.dbuser.domain;

import java.time.LocalDateTime;

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

	LocalDateTime getCreated();

	String getCreatedBy();

	LocalDateTime getModified();

	String getModifiedBy();

}
