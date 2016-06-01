package de.chandre.admintool.filebrowser;

public class DownloadNotAllowedException extends Exception {

	private static final long serialVersionUID = 1L;
	
	public DownloadNotAllowedException() {
		super();
	}
	
	public DownloadNotAllowedException(String message) {
		super(message);
	}
	
	public DownloadNotAllowedException(Throwable trowable) {
		super(trowable);
	}
	
	public DownloadNotAllowedException(String message, Throwable trowable) {
		super(message, trowable);
	}
}
