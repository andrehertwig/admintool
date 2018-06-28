package de.chandre.admintool.security.dbuser.service.comm;

/**
 * 
 * @author André
 * @since 1.1.7
 *
 */
public class SendException extends Exception {
	private static final long serialVersionUID = 6773428392816080970L;
	
	public SendException() {
		super();
	}
	
	public SendException(String message) {
		super(message);
	}
	
	public SendException(Throwable cause) {
		super(cause);
	}
	
	public SendException(String message, Throwable cause) {
		super(message, cause);
	}

}
