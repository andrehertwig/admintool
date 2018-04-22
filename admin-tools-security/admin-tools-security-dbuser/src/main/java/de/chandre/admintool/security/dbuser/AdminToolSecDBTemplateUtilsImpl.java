package de.chandre.admintool.security.dbuser;

import java.util.Locale;
import java.util.Set;
import java.util.TimeZone;
import java.util.stream.Collectors;

import org.apache.commons.lang3.LocaleUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 
 * @author André
 * @since 1.1.7
 *
 */
@Component("adminToolSecDBTemplateUtils")
public class AdminToolSecDBTemplateUtilsImpl implements AdminToolSecDBTemplateUtils {

	@Autowired 
	private AdminToolSecDBProperties properties;
	
	@Override
	public String[] getAvailableTimeZones() {
		return TimeZone.getAvailableIDs();
	}
	
	@Override
	public String getDefaultTimeZone() {
		return TimeZone.getDefault().getID();
	}
	
	@Override
	public Set<Locale> getAvailableLocales() {
		
		return properties.getUsers().getAvailableLocales().stream().map(LocaleUtils::toLocale).collect(Collectors.toSet());
	}
}
