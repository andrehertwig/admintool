package de.chandre.admintool.filebrowser;

import java.io.File;

/**
 * 
 * @author André
 * @since 1.1.7
 *
 */
public interface ATFilebrowserPermissionHandler {
	
	boolean isAllowed(File path, boolean write);
	
	boolean isCreateFolderAllowed(String currentDir);

	boolean isUploadFileAllowed(String currentDir);

	boolean isDeleteFileAllowed(File currentFile);

	boolean isDeleteFolderAllowed(File currentDir);
	
}
