package de.chandre.admintool.core;

/**
 * configuration component for admintool
 * @author Andre
 *
 */
public interface AdminToolConfig 
{
	public static final String WEBJARS_CDN_PREFIX = "https://cdn.jsdelivr.net/webjars/";
	
	public static final String WEBJARS_CDN_PREFIX_BOWER = WEBJARS_CDN_PREFIX + "org.webjars.bower/";
	
	public static final String WEBJARS_LOCAL_PREFIX = "/webjars/";
	
	/**
	 * should print the configuration to log
	 */
	public void printConfig();
	
	/**
	 * should return if component is active or deactivated
	 * @return
	 */
	public boolean isEnabled();
}
