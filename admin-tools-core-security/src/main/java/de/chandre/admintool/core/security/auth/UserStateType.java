package de.chandre.admintool.core.security.auth;

import java.util.Locale;

/**
 * spring security states of user shown in user view
 * 
 * @author Andre
 * @since 1.1.5
 */
public enum UserStateType {
	
	/**
	 * if account is enabled
	 */
	ENABLE("enable"),
	/**
	 * if account is expired
	 */
	EXPIRE("expire"),
	/**
	 * if account is locked
	 */
	LOCK("lock"),
	/**
	 * if account has expired credentials
	 */
	EXIPRE_CREDENTIALS("expcred");
	
	private String type;
	private UserStateType(String type) {
		this.type = type;
	}
	
	public static UserStateType fromType(String type) {
		if (null != type) {
			for (UserStateType userStateType : values()) {
				if (userStateType.type.equals(type.toLowerCase(Locale.ENGLISH))) {
					return userStateType;
				}
			}
		}
		throw new IllegalArgumentException("unknown user state type: " + type);
	}

	public String getType() {
		return type;
	}
	
}
