package de.chandre.admintool.db;

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
public class AdminToolDBBrowserRoles implements AdminToolRoles {
	
	public static String ROLE_DBBROWSER = "DBBROWSER";
	
	@Override
	public Collection<String> getRoles() {
		return Collections.unmodifiableList(Arrays.asList(ROLE_DBBROWSER));
	}
	
}
