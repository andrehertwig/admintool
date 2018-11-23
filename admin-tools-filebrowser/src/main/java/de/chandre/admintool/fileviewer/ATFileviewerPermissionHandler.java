package de.chandre.admintool.fileviewer;

import java.io.File;

import de.chandre.admintool.filebrowser.GenericFilebrowserException;

/**
 * 
 * @author André
 * @since 1.2.0
 *
 */
public interface ATFileviewerPermissionHandler {
	
	boolean isAllowed(File path, boolean write, AdminToolFileviewerService delegate) throws GenericFilebrowserException;
	
	/**
	 * CAUTION: do NOT call delegate.isChangeable. This will lead to circular calls and will end in Stack-Overflow-Exception
	 * @param file
	 * @param delegate
	 * @return
	 */
	boolean isChangeable(File file, AdminToolFileviewerService delegate);
	
}
