package de.chandre.admintool.core;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
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
	private static final Log LOGGER = LogFactory.getLog(AdminToolCoreConfig.class);
	
	@Value("${admintool.core.enabled:true}")
	private boolean enabled;
	
	@Value("${admintool.core.useCDN:true}")
	private boolean useCDNs;
	
	@Value("${admintool.core.adminLTE.cdn.version:2.3.3}")
	private String adminLTECdnVersion;
	
	@Value("${admintool.core.showStacktraceOnErrorPage:true}")
	private boolean showStacktraceOnErrorPage;
	
	
	
	@Override
	public boolean isEnabled() {
		return enabled;
	}

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

	/**
	 * @return the showStacktraceOnErrorPage
	 */
	public boolean isShowStacktraceOnErrorPage() {
		return showStacktraceOnErrorPage;
	}

	/**
	 * @param showStacktraceOnErrorPage the showStacktraceOnErrorPage to set
	 */
	public void setShowStacktraceOnErrorPage(boolean showStacktraceOnErrorPage) {
		this.showStacktraceOnErrorPage = showStacktraceOnErrorPage;
	}

	@Override
	public void printConfig() {
		LOGGER.debug(toString());
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AdminToolCoreConfig [enabled=").append(enabled).append(", useCDNs=").append(useCDNs)
				.append(", adminLTECdnVersion=").append(adminLTECdnVersion).append(", showStacktraceOnErrorPage=")
				.append(showStacktraceOnErrorPage).append("]");
		return builder.toString();
	}
}
