package de.chandre.admintool.fileviewer;

import java.io.File;

/**
 * 
 * @author André
 * @since 1.1.7
 *
 */
public interface ATFileviewerPermissionHandler {
	
	boolean isAllowed(File path, boolean write);
	
	boolean isChangeable(File file);
	
}
