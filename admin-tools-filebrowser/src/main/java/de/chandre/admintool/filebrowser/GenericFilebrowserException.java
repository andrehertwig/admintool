package de.chandre.admintool.filebrowser;

public class GenericFilebrowserException extends Exception {

	private static final long serialVersionUID = 1L;
	
	public GenericFilebrowserException() {
		super();
	}
	
	public GenericFilebrowserException(String message) {
		super(message);
	}
	
	public GenericFilebrowserException(Throwable trowable) {
		super(trowable);
	}
	
	public GenericFilebrowserException(String message, Throwable trowable) {
		super(message, trowable);
	}
}
