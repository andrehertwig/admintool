package de.chandre.admintool.security.dbuser.auth;

import java.io.Serializable;

/**
 * 
 * @author André
 * @since 1.1.7
 *
 */
public class PasswordTO implements Serializable {
	private static final long serialVersionUID = 7282206856603860917L;
	
	private String currentPassword;
	private String newPassword;
	private String passwordConfirm;
	
	public String getCurrentPassword() {
		return currentPassword;
	}
	public void setCurrentPassword(String currentPassword) {
		this.currentPassword = currentPassword;
	}
	public String getNewPassword() {
		return newPassword;
	}
	public void setNewPassword(String newPassword) {
		this.newPassword = newPassword;
	}
	public String getPasswordConfirm() {
		return passwordConfirm;
	}
	public void setPasswordConfirm(String passwordConfirm) {
		this.passwordConfirm = passwordConfirm;
	}
}
