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
	
	@Value("${admintool.dbbrowser.dmlAllowed:false}")
	private boolean dmlAllowed;
	
	@Value("${admintool.dbbrowser.dmlBackdoorAllowed:false}")
	private boolean dmlBackdoorAllowed;
	
	@Value("${admintool.dbbrowser.clobEncodings:}")
	private List<String> clobEncodings = new ArrayList<>();
	
	private ExampleStatements exampleStatements;

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
	 * @return the exampleStatements
	 */
	public ExampleStatements getExampleStatements() {
		return exampleStatements;
	}

	/**
	 * @param exampleStatements the exampleStatements to set
	 */
	public void setExampleStatements(ExampleStatements exampleStatements) {
		this.exampleStatements = exampleStatements;
	}
}
