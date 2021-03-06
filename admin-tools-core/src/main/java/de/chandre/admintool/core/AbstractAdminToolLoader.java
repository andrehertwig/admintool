package de.chandre.admintool.core;

import org.springframework.beans.factory.annotation.Autowired;

/**
 * Abstract class which provides the core configuration bean and 
 * some useful methods for URI prefixes of CDN / local CSS or JS files
 * 
 * @author Andre
 * @since 1.0.0
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
	 * if param = true {@link #getWebjarsBowerPrefixUri()} will be called otherwise {@link #getWebjarsPrefixUri()}
	 * @param useBower
	 * @return
	 * @since 1.1.4
	 */
	protected String getWebjarsPrefixUri(boolean useBower) {
		if (useBower) {
			return getWebjarsBowerPrefixUri();
		}
		return getWebjarsPrefixUri();
	}
	
	/**
	 * 
	 * @return either the relative URL to included webjar (if {@link #shouldCDNsUsed()} is false) or
	 * the absolute URL to webjars CDN {@link AdminToolConfig#WEBJARS_CDN_PREFIX}
	 */
	protected String getWebjarsPrefixUri() {
		if (coreConfig.isUseCDNs()) {
			return AdminToolConfig.WEBJARS_CDN_PREFIX;
		}
		return AdminToolConfig.WEBJARS_LOCAL_PREFIX;
	}
	
	/**
	 * 
	 * @return either the relative URL to included webjar (if {@link #shouldCDNsUsed()} is false) or
	 * the absolute URL to webjars CDN with bower {@link AdminToolConfig#WEBJARS_CDN_PREFIX_BOWER}
	 * @since 1.1.4
	 */
	protected String getWebjarsBowerPrefixUri() {
		if (coreConfig.isUseCDNs()) {
			return AdminToolConfig.WEBJARS_CDN_PREFIX_BOWER;
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
			return AdminToolConfig.WEBJARS_CDN_PREFIX_BOWER + "adminlte/" + coreConfig.getAdminLTECdnVersion() + "/";
		}
		return AdminToolConfig.WEBJARS_LOCAL_PREFIX + "adminlte/" + coreConfig.getAdminLTECdnVersion() + "/";
	}
}
