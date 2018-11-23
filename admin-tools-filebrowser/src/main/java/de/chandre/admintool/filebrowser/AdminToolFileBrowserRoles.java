package de.chandre.admintool.filebrowser;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.springframework.stereotype.Component;

import de.chandre.admintool.core.sec.ATInitRole;
import de.chandre.admintool.core.sec.AdminToolRoles;
import de.chandre.admintool.core.sec.ATInitRole.ATInitRoleBuilder;

/**
 * 
 * @author Andre
 * @since 1.2.0
 */
@Component
public class AdminToolFileBrowserRoles implements AdminToolRoles {
	
	public static final ATInitRole ROLE_FILEBROWSER = ATInitRoleBuilder.builder()
			.name("FILEBROWSER").displayName("FileBrowser Role").description("This role is for the usage of file browser").build();
	
	public static final ATInitRole ROLE_FILEBROWSER_DOWNLOAD = ATInitRoleBuilder.builder()
			.name("FILEBROWSER_DOWNLOAD").displayName("FileBrowser Download").description("Allows download functionality").build();
	public static final ATInitRole ROLE_FILEBROWSER_DOWNLOADZIP = ATInitRoleBuilder.builder()
			.name("FILEBROWSER_DOWNLOADZIP").displayName("FileBrowser Download Zip").description("Allows compressed download functionality").build();
	public static final ATInitRole ROLE_FILEBROWSER_UPLOAD = ATInitRoleBuilder.builder()
			.name("FILEBROWSER_UPLOAD").displayName("FileBrowser Upload").description("Allows upload functionality").build();
	public static final ATInitRole ROLE_FILEBROWSER_CREATEFOLDER = ATInitRoleBuilder.builder()
			.name("FILEBROWSER_CREATEFOLDER").displayName("FileBrowser CreateFolder").description("Allows create folder functionality").build();
	public static final ATInitRole ROLE_FILEBROWSER_DELETEFOLDER = ATInitRoleBuilder.builder()
			.name("FILEBROWSER_DELETEFOLDER").displayName("FileBrowser DeleteFolder").description("Allows delete folder functionality").build();
	public static final ATInitRole ROLE_FILEBROWSER_DELETEFILE = ATInitRoleBuilder.builder()
			.name("FILEBROWSER_DELETEFILE").displayName("FileBrowser DeleteFile").description("Allows delete file functionality").build();
	
	@Override
	public Collection<ATInitRole> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_FILEBROWSER, ROLE_FILEBROWSER_DOWNLOAD, ROLE_FILEBROWSER_DOWNLOADZIP,
				ROLE_FILEBROWSER_UPLOAD, ROLE_FILEBROWSER_CREATEFOLDER, ROLE_FILEBROWSER_DELETEFOLDER, ROLE_FILEBROWSER_DELETEFILE));
	}
	
}
