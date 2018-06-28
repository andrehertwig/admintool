package de.chandre.admintool.security.dbuser.service;

import java.util.UUID;

/**
 * uses the UUID to generate a unique non-reproducible string
 * @author André
 * @since 1.1.7
 *
 */
public class DefaultPasswordLinkHashGenerator implements PasswordLinkHashGenerator {

	@Override
	public String generatePasswordLinkHash() {
		return UUID.randomUUID().toString();
	}

}
