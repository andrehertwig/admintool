package de.chandre.admintool.db;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * configuration class for adminTool dbbrowser
 * @author Andre
 *
 */
@Component
public class AdminToolDBBrowserConfig
{
	@Value("${admintool.dbbrowser.dmlAllowed:false}")
	private boolean dmlAllowed;
	
	@Value("${admintool.dbbrowser.dmlBackdoorAllowed:false}")
	private boolean dmlBackdoorAllowed;

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
	 * @return the dmlBackdoorAllowed
	 */
	public boolean isDmlBackdoorAllowed() {
		return dmlBackdoorAllowed;
	}

	/**
	 * @param dmlBackdoorAllowed the dmlBackdoorAllowed to set
	 */
	public void setDmlBackdoorAllowed(boolean dmlBackdoorAllowed) {
		this.dmlBackdoorAllowed = dmlBackdoorAllowed;
	}
	
}
