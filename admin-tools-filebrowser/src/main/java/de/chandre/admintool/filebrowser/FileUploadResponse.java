package de.chandre.admintool.filebrowser;

import java.io.Serializable;

/**
 * response class file for uploading via fine-uploader
 *  
 * @author André 
 * @since 1.1.6
 */
public class FileUploadResponse implements Serializable {
	private static final long serialVersionUID = 1943209281237112388L;
	
	private boolean success;
	private String error;
	private boolean preventRetry;
	private String newUuid;
	public boolean isSuccess() {
		return success;
	}
	public void setSuccess(boolean success) {
		this.success = success;
	}
	public String getError() {
		return error;
	}
	public void setError(String error) {
		this.error = error;
	}
	public boolean isPreventRetry() {
		return preventRetry;
	}
	public void setPreventRetry(boolean preventRetry) {
		this.preventRetry = preventRetry;
	}
	public String getNewUuid() {
		return newUuid;
	}
	public void setNewUuid(String newUuid) {
		this.newUuid = newUuid;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((error == null) ? 0 : error.hashCode());
		result = prime * result + ((newUuid == null) ? 0 : newUuid.hashCode());
		result = prime * result + (preventRetry ? 1231 : 1237);
		result = prime * result + (success ? 1231 : 1237);
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
		FileUploadResponse other = (FileUploadResponse) obj;
		if (error == null) {
			if (other.error != null)
				return false;
		} else if (!error.equals(other.error))
			return false;
		if (newUuid == null) {
			if (other.newUuid != null)
				return false;
		} else if (!newUuid.equals(other.newUuid))
			return false;
		if (preventRetry != other.preventRetry)
			return false;
		if (success != other.success)
			return false;
		return true;
	}
	
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("FileUploadResponse [success=").append(success).append(", error=").append(error)
				.append(", preventRetry=").append(preventRetry).append(", newUuid=").append(newUuid).append("]");
		return builder.toString();
	}
	
}
