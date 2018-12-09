package de.chandre.admintool.security.dbuser;

import java.util.Locale;
import java.util.Set;

/**
 * 
 * @author André
 * @since 1.2.0
 *
 */
public interface AdminToolSecDBTemplateUtils {

	String[] getAvailableTimeZones();

	Set<Locale> getAvailableLocales();

	String getDefaultTimeZone();

	boolean isDirectPasswordChangeAllowed();

	boolean isDirectPasswordChangeInProfileAllowed();

	boolean isCommunicatorImplemented();

}
