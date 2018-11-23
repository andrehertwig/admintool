package de.chandre.admintool.fileviewer;

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
public class AdminToolFileViewerRoles implements AdminToolRoles {
	
	public static final ATInitRole ROLE_FILEVIEWER = ATInitRoleBuilder.builder()
			.name("FILEVIEWER").displayName("FileViewer Role").description("This role is for the usage of file viewer").build();
	
	public static final ATInitRole ROLE_FILEVIEWER_CHANGE = ATInitRoleBuilder.builder()
			.name("FILEVIEWER_CHANGE").displayName("FileViewer Change").description("Allows change file functionality").build();
	
	@Override
	public Collection<ATInitRole> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_FILEVIEWER, ROLE_FILEVIEWER_CHANGE));
	}
	
}
