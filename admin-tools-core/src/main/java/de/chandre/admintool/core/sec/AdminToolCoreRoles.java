package de.chandre.admintool.core.sec;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.springframework.stereotype.Component;

import de.chandre.admintool.core.sec.AdminToolRoles;

/**
 * 
 * @author Andre
 * @since 1.2.0
 */
@Component
public class AdminToolCoreRoles implements AdminToolRoles {
	
	public static String ROLE_ATCORE = "ATCORE";
	
	@Override
	public Collection<String> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_ATCORE));
	}
	
}
