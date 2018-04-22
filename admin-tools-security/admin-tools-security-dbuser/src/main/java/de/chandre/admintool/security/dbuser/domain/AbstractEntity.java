package de.chandre.admintool.security.dbuser.domain;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.EntityListeners;
import javax.persistence.Id;
import javax.persistence.MappedSuperclass;
import javax.persistence.Version;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

/**
 * abstract class for all entities with common fields and UUID as Id
 * requires @EnableJpaAuditing in JPA configuration
 * 
 * @author André
 * @since 1.1.7
 */
@SuppressWarnings("serial")
@MappedSuperclass
@EntityListeners(AuditingEntityListener.class)
public abstract class AbstractEntity implements Serializable, Entity {
	
	@Id
	@Column(name="UUID", length=40, nullable=false, unique=true)
	private String id;
	
	@Version
	@Column(name="JPAVERSION", nullable=false)
	private Integer version;
	
	@CreatedDate
	@Column(name="CREATED", nullable=false)
	private LocalDateTime created;
	
	@CreatedBy
	@Column(name="CREATED_BY", nullable=true)
	private String createdBy;
	
	@LastModifiedDate
	@Column(name="MODIFIED", nullable=true)
	private LocalDateTime modified;
	
	@LastModifiedBy
	@Column(name="MODIFIED_BY", nullable=true)
	private String modifiedBy;
	
	public AbstractEntity() {
		setId(UUID.randomUUID().toString());
		setCreated(LocalDateTime.now());
	}

	@Override
	public String getId() {
		return id;
	}

	@Override
	public boolean isNew() {
		return null == id || version == null;
	}

	@Override
	public Integer getVersion() {
		return version;
	}

	public void setVersion(Integer version) {
		this.version = version;
	}

	@Override
	public LocalDateTime getCreated() {
		return created;
	}

	public void setCreated(LocalDateTime created) {
		this.created = created;
	}
	
	@Override
	public String getCreatedBy() {
		return createdBy;
	}

	public void setCreatedBy(String createdBy) {
		this.createdBy = createdBy;
	}

	@Override
	public LocalDateTime getModified() {
		return modified;
	}

	public void setModified(LocalDateTime modified) {
		this.modified = modified;
	}

	@Override
	public String getModifiedBy() {
		return modifiedBy;
	}

	public void setModifiedBy(String modifiedBy) {
		this.modifiedBy = modifiedBy;
	}

	public void setId(String id) {
		this.id = id;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((created == null) ? 0 : created.hashCode());
		result = prime * result + ((id == null) ? 0 : id.hashCode());
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
		AbstractEntity other = (AbstractEntity) obj;
		if (created == null) {
			if (other.created != null)
				return false;
		} else if (!created.equals(other.created))
			return false;
		if (id == null) {
			if (other.id != null)
				return false;
		} else if (!id.equals(other.id))
			return false;
		return true;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AbstractEntity [id=").append(id).append(", version=").append(version).append(", created=")
				.append(created).append(", createdBy=").append(createdBy).append(", modified=").append(modified)
				.append(", modifiedBy=").append(modifiedBy).append("]");
		return builder.toString();
	}
}
