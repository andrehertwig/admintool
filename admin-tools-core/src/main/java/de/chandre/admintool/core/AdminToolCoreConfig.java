package de.chandre.admintool.core;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * configuration component for admintool
 * @author Andre
 *
 */
@Component("adminToolConfig")
public class AdminToolCoreConfig implements AdminToolConfig
{
	@Value("${admintool.core.useCDN:true}")
	private boolean useCDNs;
	
	@Value("${admintool.core.adminLTE.cdn.version:2.3.3}")
	private String adminLTECdnVersion;

	/** 
	 * if set to false webJars included in in packaging will be used. useful for workplaces with not Internet access
	 * @return the useCDNs
	 */
	public boolean isUseCDNs() {
		return useCDNs;
	}

	/**
	 * if set to false webJars included in in packaging will be used. useful for workplaces with not Internet access
	 * @param useCDNs the useCDNs to set
	 */
	public void setUseCDNs(boolean useCDNs) {
		this.useCDNs = useCDNs;
	}

	/**
	 * the version to use of adminLTE in CDN of http://www.webjars.org/
	 * @return the adminLTECdnVersion
	 */
	public String getAdminLTECdnVersion() {
		return adminLTECdnVersion;
	}

	/**
	 * the version to use of adminLTE in CDN of http://www.webjars.org/
	 * @param adminLTECdnVersion the adminLTECdnVersion to set
	 */
	public void setAdminLTECdnVersion(String adminLTECdnVersion) {
		this.adminLTECdnVersion = adminLTECdnVersion;
	}
}
