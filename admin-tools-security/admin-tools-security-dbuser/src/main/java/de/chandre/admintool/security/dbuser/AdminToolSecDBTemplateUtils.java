package de.chandre.admintool.security.dbuser;

import java.util.Locale;
import java.util.Set;

public interface AdminToolSecDBTemplateUtils {

	String[] getAvailableTimeZones();

	Set<Locale> getAvailableLocales();

	String getDefaultTimeZone();

}
