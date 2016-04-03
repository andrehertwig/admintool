package de.chandre.admintool.core;

import org.springframework.beans.factory.annotation.Autowired;

public abstract class AbstractAdminToolLoader
{
	@Autowired
	private AdminToolCoreConfig config;
	
	/**
	 * 
	 * @return the configuration if CDNs should be used
	 */
	protected boolean shouldCDNsUsed() {
		return config.isUseCDNs();
	}
	
	/**
	 * 
	 * @return either the relative URL to included webjar (if {@link #shouldCDNsUsed()} is false) or
	 * the absolute URL to webjars CDN
	 */
	protected String getWebjarsPrefixUri() {
		if (config.isUseCDNs()) {
			return AdminToolConfig.WEBJARS_CDN_PREFIX;
		}
		return AdminToolConfig.WEBJARS_LOCAL_PREFIX;
	}
	
	/**
	 * 
	 * @return either the relative URL to included webjar of adminLTE to configured version (if {@link #shouldCDNsUsed()} is false) or
	 * the absolute URL to webjars CDN of admintool
	 */
	protected String getAdminLTEPrefixUri() {
		if (config.isUseCDNs()) {
			return AdminToolConfig.WEBJARS_CDN_PREFIX + "org.webjars.bower/adminlte/" + config.getAdminLTECdnVersion() + "/";
		}
		return AdminToolConfig.WEBJARS_LOCAL_PREFIX + "adminlte/" + config.getAdminLTECdnVersion() + "/";
	}
}
