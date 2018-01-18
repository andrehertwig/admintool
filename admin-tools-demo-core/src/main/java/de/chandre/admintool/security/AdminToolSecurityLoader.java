package de.chandre.admintool.security;

import javax.annotation.PostConstruct;

import org.springframework.stereotype.Component;

import de.chandre.admintool.security.simple.auth.AbstractAdminToolSecurityViewLoader;

@Component
public class AdminToolSecurityLoader extends AbstractAdminToolSecurityViewLoader {

	@PostConstruct
	public void init() {
		super.addUsersMenu();
	}
	
}
