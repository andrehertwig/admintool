package de.chandre.admintool.fileviewer;

import java.io.File;

import de.chandre.admintool.core.sec.ATAbstractPermissionHandler;
import de.chandre.admintool.filebrowser.GenericFilebrowserException;

/**
 * 
 * @author André
 * @since 1.2.0
 *
 */
public class ATFileviewerDefaultPermissionHandler extends ATAbstractPermissionHandler implements ATFileviewerPermissionHandler {

	@Override
	public boolean isAllowed(File path, boolean write, AdminToolFileviewerService delegate) throws GenericFilebrowserException {
		delegate.isFileAllowed(path, write);
		return true;
	}

	@Override
	public boolean isChangeable(File file, AdminToolFileviewerService delegate) {
		return delegate.isExtensionAllowedAndWriteable(file) && userHasRole(AdminToolFileViewerRoles.ROLE_FILEVIEWER_CHANGE);
	}
	
}
