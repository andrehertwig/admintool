package de.chandre.admintool.db;

import java.util.ArrayList;
import java.util.List;

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
	
	@Value("${admintool.dbbrowser.codeMirrorVersion:5.13.2}")
	private String codeMirrorVersion;
	
	@Value("#{'${admintool.dbbrowser.codeMirrorAddLibs:addon/edit/matchbrackets.js}'.split(';')}")
	private List<String> codeMirrorAddLibs = new ArrayList<>();
	
	/**
	 * @return the enabled
	 */
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

	@Override
	@PostConstruct
	public void printConfig() {
		LOGGER.debug(toString());
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AdminToolDBBrowserConfig [enabled=").append(enabled).append(", hideMenuItem=")
				.append(hideMenuItem).append(", dmlAllowed=").append(dmlAllowed).append(", clobEncodings=")
				.append(clobEncodings).append(", codeMirrorVersion=").append(codeMirrorVersion)
				.append(", codeMirrorAddLibs=").append(codeMirrorAddLibs).append("]");
		return builder.toString();
	}
	
}
