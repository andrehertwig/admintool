package de.chandre.admintool.db;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

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
	
	@Value("${admintool.dbbrowser.dmlAllowed:false}")
	private boolean dmlAllowed;
	
	@Value("${admintool.dbbrowser.clobEncodings:}")
	private List<String> clobEncodings = new ArrayList<>();
	
	@Value("${admintool.dbbrowser.codeMirrorVersion:5.13.2}")
	private String codeMirrorVersion;

	@PostConstruct
	public void setDefaults() {
		if (clobEncodings.isEmpty()
				|| (clobEncodings.size() == 1 && StringUtils.isEmpty(clobEncodings.iterator().next()))) {
			LOGGER.info("using default clob encoding configuration");
			clobEncodings = new ArrayList<>();
			clobEncodings.add("UTF-8");
			clobEncodings.add("ISO-8859-1");
		}
		if (LOGGER.isDebugEnabled())
			LOGGER.debug("clob encoding is empty: " + clobEncodings.isEmpty() + ", size: " + clobEncodings.size());
	}
	
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
	
}
