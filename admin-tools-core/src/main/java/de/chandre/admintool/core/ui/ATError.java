package de.chandre.admintool.core.ui;

import java.io.Serializable;

/**
 * convenient transfer object for error messages
 * 
 * @author André
 * @since 1.1.7
 */
public class ATError implements Serializable {
	private static final long serialVersionUID = -4972646697107352412L;
	
	private String key;
	private String message;
	
	private String field;
	
	public ATError() {
		super();
	}
	
	public ATError(String key, String message) {
		this(key, message, null);
	}

	public ATError(String key, String message, String field) {
		super();
		this.key = key;
		this.message = message;
		this.field = field;
	}

	public String getKey() {
		return key;
	}
	public void setKey(String key) {
		this.key = key;
	}
	public String getMessage() {
		return message;
	}
	public void setMessage(String message) {
		this.message = message;
	}
	
	public String getField() {
		return field;
	}

	public void setField(String field) {
		this.field = field;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((field == null) ? 0 : field.hashCode());
		result = prime * result + ((key == null) ? 0 : key.hashCode());
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
		ATError other = (ATError) obj;
		if (field == null) {
			if (other.field != null)
				return false;
		} else if (!field.equals(other.field))
			return false;
		if (key == null) {
			if (other.key != null)
				return false;
		} else if (!key.equals(other.key))
			return false;
		if (message == null) {
			if (other.message != null)
				return false;
		} else if (!message.equals(other.message))
			return false;
		return true;
	}
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("ATError [key=").append(key).append(", message=").append(message).append(", field=")
				.append(field).append("]");
		return builder.toString();
	}
}
