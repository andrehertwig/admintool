package de.chandre.admintool.security;

import org.springframework.security.config.annotation.authentication.ProviderManagerBuilder;
import org.springframework.security.config.annotation.authentication.configurers.provisioning.InMemoryUserDetailsManagerConfigurer;
import org.springframework.security.config.annotation.authentication.configurers.provisioning.UserDetailsManagerConfigurer;

public class AdminToolInMemoryUserDetailsConfigurer<B extends ProviderManagerBuilder<B>>
		extends UserDetailsManagerConfigurer<B, InMemoryUserDetailsManagerConfigurer<B>> {
	
	public AdminToolInMemoryUserDetailsConfigurer(AdminToolInMemoryUserDetailsService userDetailsService) {
		super(userDetailsService);
	}
	
	@Override
	public AdminToolInMemoryUserDetailsService getUserDetailsService() {
		return (AdminToolInMemoryUserDetailsService) super.getUserDetailsService();
	}
}
