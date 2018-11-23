package de.chandre.admintool.filebrowser;

import java.io.File;
import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;

import de.chandre.admintool.core.sec.ATAbstractPermissionHandler;

/**
 * 
 * @author Andre
 * @since 1.2.0
 */
public class ATFilebrowserDefaultPermissionHandler extends ATAbstractPermissionHandler implements ATFilebrowserPermissionHandler {
	
	@Autowired
	private AdminToolFilebrowserConfig config;
	

	@Override
	public boolean isAllowed(File path, boolean write, AdminToolFilebrowserService delegate) throws IOException {
		return delegate.isAllowed(path, write, config.isReadOnly());
	}

	@Override
	public boolean isCreateFolderAllowed(String currentDir) {
		return config.isCreateFolderAllowed() && userHasRole(AdminToolFileBrowserRoles.ROLE_FILEBROWSER_CREATEFOLDER);
	}

	@Override
	public boolean isUploadFileAllowed(String currentDir) {
		return config.isUploadAllowed() && userHasRole(AdminToolFileBrowserRoles.ROLE_FILEBROWSER_UPLOAD);
	}

	@Override
	public boolean isDeleteFileAllowed(File currentFile) {
		return config.isDeleteFileAllowed() && userHasRole(AdminToolFileBrowserRoles.ROLE_FILEBROWSER_DELETEFILE);
	}

	@Override
	public boolean isDeleteFolderAllowed(File currentDir) {
		return config.isDeleteFolderAllowed() && userHasRole(AdminToolFileBrowserRoles.ROLE_FILEBROWSER_DELETEFOLDER);
	}

	@Override
	public boolean isDownloadAllowed(File currentDir) {
		return config.isDownloadAllowed() && userHasRole(AdminToolFileBrowserRoles.ROLE_FILEBROWSER_DOWNLOAD);
	}
	
	@Override
	public boolean isDownloadCompressedAllowed(File currentDir) {
		return config.isDownloadCompressedAllowed() && userHasRole(AdminToolFileBrowserRoles.ROLE_FILEBROWSER_DOWNLOADZIP);
	}
}
