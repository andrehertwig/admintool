package de.chandre.admintool.filebrowser;

import java.io.File;

/**
 * 
 * @author Andre
 * @since 1.2.0
 */
public interface ATFileBrowserPermissions {
	
	boolean isCreateFolderAllowed(String currentDir);

	boolean isUploadFileAllowed(String currentDir);

	boolean isDeleteFileAllowed(File currentFile);

	boolean isDeleteFolderAllowed(File currentDir);

	boolean isDownloadAllowed(File currentDir);

	boolean isDownloadCompressedAllowed(File currentDir);

}
