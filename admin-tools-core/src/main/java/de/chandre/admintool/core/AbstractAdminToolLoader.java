package de.chandre.admintool.core;

import org.springframework.beans.factory.annotation.Autowired;

/**
 * 
 * @author Andre
 *
 */
public abstract class AbstractAdminToolLoader
{
	@Autowired
	protected AdminToolCoreConfig coreConfig;
	
	/**
	 * 
	 * @return the configuration if CDNs should be used
	 */
	protected boolean shouldCDNsUsed() {
		return coreConfig.isUseCDNs();
	}
	
	/**
	 * 
	 * @return either the relative URL to included webjar (if {@link #shouldCDNsUsed()} is false) or
	 * the absolute URL to webjars CDN
	 */
	protected String getWebjarsPrefixUri() {
		if (coreConfig.isUseCDNs()) {
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
		if (coreConfig.isUseCDNs()) {
			return AdminToolConfig.WEBJARS_CDN_PREFIX + "org.webjars.bower/adminlte/" + coreConfig.getAdminLTECdnVersion() + "/";
		}
		return AdminToolConfig.WEBJARS_LOCAL_PREFIX + "adminlte/" + coreConfig.getAdminLTECdnVersion() + "/";
	}
}
