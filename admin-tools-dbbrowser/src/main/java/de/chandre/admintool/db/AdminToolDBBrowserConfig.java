package de.chandre.admintool.db;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import de.chandre.admintool.core.AdminToolConfig;

/**
 * configuration class for adminTool dbbrowser
 * @author Andre
 *
 */
@Component("adminToolDBBrowserConfig")
public class AdminToolDBBrowserConfig implements AdminToolConfig
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolDBBrowserConfig.class);
	
	@Value("${admintool.dbbrowser.enabled:true}")
	private boolean enabled;
	
	@Value("${admintool.dbbrowser.hideMenuItem:false}")
	private boolean hideMenuItem;
	
	@Value("${admintool.dbbrowser.dmlAllowed:false}")
	private boolean dmlAllowed;
	
	@Value("#{'${admintool.dbbrowser.clobEncodings:UTF-8;ISO-8859-1}'.split(';')}")
	private List<String> clobEncodings = new ArrayList<>();
	
	@Value("${admintool.dbbrowser.codeMirrorVersion:5.22.2}")
	private String codeMirrorVersion;
	
	@Value("${admintool.dbbrowser.codeMirror.cdn.useBower:true}")
	private boolean codeMirrorUseBowser;
	
	@Value("#{'${admintool.dbbrowser.codeMirrorAddLibs:addon/edit/matchbrackets.js}'.split(';')}")
	private List<String> codeMirrorAddLibs = new ArrayList<>();
	
	@Value("#{'${admintool.dbbrowser.securityRoles:}'.split(';')}")
	private Set<String> securityRoles = new HashSet<>();
	
	@Value("${admintool.dbbrowser.componentPosition:}")
	private Integer componentPosition;
	
	@Value("${admintool.dbbrowser.showMetaDataLoadException:false}")
	private boolean showMetaDataLoadException;
	
	/**
	 * @return the enabled
	 */
	@Override
	public boolean isEnabled() {
		return enabled;
	}

	/**
	 * @param enabled the enabled to set
	 */
	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	/**
	 * @return the hideMenuItem
	 */
	public boolean isHideMenuItem() {
		return hideMenuItem;
	}

	/**
	 * @param hideMenuItem the hideMenuItem to set
	 */
	public void setHideMenuItem(boolean hideMenuItem) {
		this.hideMenuItem = hideMenuItem;
	}

	/**
	 * @return the dmlAllowed
	 */
	public boolean isDmlAllowed() {
		return dmlAllowed;
	}

	/**
	 * @param dmlAllowed the dmlAllowed to set
	 */
	public void setDmlAllowed(boolean dmlAllowed) {
		this.dmlAllowed = dmlAllowed;
	}
	
	/**
	 * @return the clobEncodings
	 */
	public List<String> getClobEncodings() {
		return clobEncodings;
	}

	/**
	 * @param clobEncodings the clobEncodings to set
	 */
	public void setClobEncodings(List<String> clobEncodings) {
		this.clobEncodings = clobEncodings;
	}

	/**
	 * @return the codeMirrorVersion
	 */
	public String getCodeMirrorVersion() {
		return codeMirrorVersion;
	}

	/**
	 * @param codeMirrorVersion the codeMirrorVersion to set
	 */
	public void setCodeMirrorVersion(String codeMirrorVersion) {
		this.codeMirrorVersion = codeMirrorVersion;
	}

	/**
	 * if {@link AdminToolConfig#WEBJARS_CDN_PREFIX_BOWER} should be used or {@link AdminToolConfig#WEBJARS_CDN_PREFIX}
	 * @return
	 * @since 1.1.4
	 */
	public boolean getCodeMirrorUseBowser() {
		return codeMirrorUseBowser;
	}

	/**
	 * 
	 * if {@link AdminToolConfig#WEBJARS_CDN_PREFIX_BOWER} should be used or {@link AdminToolConfig#WEBJARS_CDN_PREFIX}
	 * @param codeMirrorUseBowser
	 * @since 1.1.4
	 */
	public void setCodeMirrorUseBowser(boolean codeMirrorUseBowser) {
		this.codeMirrorUseBowser = codeMirrorUseBowser;
	}

	/**
	 * @return the codeMirrorAddLibs
	 */
	public List<String> getCodeMirrorAddLibs() {
		return codeMirrorAddLibs;
	}

	/**
	 * @param codeMirrorAddLibs the codeMirrorAddLibs to set
	 */
	public void setCodeMirrorAddLibs(List<String> codeMirrorAddLibs) {
		this.codeMirrorAddLibs = codeMirrorAddLibs;
	}

	/**
	 * @return the securityRoles
	 * @since 1.0.1
	 */
	public Set<String> getSecurityRoles() {
		return securityRoles;
	}

	/**
	 * @param securityRoles the securityRoles to set
	 * @since 1.0.1
	 */
	public void setSecurityRoles(Set<String> securityRoles) {
		this.securityRoles = securityRoles;
	}

	/**
	 * @return the componentPosition
	 * @since 1.0.1
	 */
	public Integer getComponentPosition() {
		return componentPosition;
	}

	/**
	 * @param componentPosition the componentPosition to set
	 * @since 1.0.1
	 */
	public void setComponentPosition(Integer componentPosition) {
		this.componentPosition = componentPosition;
	}
	
	
	/**
	 * 
	 * @return
	 * @since 1.1.3
	 */
	public boolean isShowMetaDataLoadException() {
		return showMetaDataLoadException;
	}

	/**
	 * 
	 * @param showMetaDataLoadException
	 * @since 1.1.3
	 */
	public void setShowMetaDataLoadException(boolean showMetaDataLoadException) {
		this.showMetaDataLoadException = showMetaDataLoadException;
	}

	@Override
	@PostConstruct
	public void printConfig() {
		LOGGER.debug(toString());
	}
	
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AdminToolDBBrowserConfig [enabled=").append(enabled).append(", hideMenuItem=")
				.append(hideMenuItem).append(", dmlAllowed=").append(dmlAllowed).append(", clobEncodings=")
				.append(clobEncodings).append(", codeMirrorVersion=").append(codeMirrorVersion)
				.append(", codeMirrorUseBowser=").append(codeMirrorUseBowser).append(", codeMirrorAddLibs=")
				.append(codeMirrorAddLibs).append(", securityRoles=").append(securityRoles)
				.append(", componentPosition=").append(componentPosition).append(", showMetaDataLoadException=")
				.append(showMetaDataLoadException).append("]");
		return builder.toString();
	}
	
}
