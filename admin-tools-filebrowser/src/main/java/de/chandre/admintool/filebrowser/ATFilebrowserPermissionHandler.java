package de.chandre.admintool.filebrowser;

import java.io.File;
import java.io.IOException;

/**
 * 
 * @author Andre
 * @since 1.2.0
 */
public interface ATFilebrowserPermissionHandler extends ATFileBrowserPermissions{
	
	boolean isAllowed(File path, boolean write, AdminToolFilebrowserService delegate) throws IOException;
}
