package de.chandre.admintool.core.thymeleaf;

import java.util.Comparator;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Comparator expects the full url path of template<br>
 * Templates found in core will have the lowest order
 * @author Andre
 * @since 1.0.1
 *
 */
public class TemplateUrlComparator implements Comparator<String> {
	
	private static final Log LOGGER = LogFactory.getLog(TemplateUrlComparator.class);
	
	private static final Pattern[] CORE_LIB = new Pattern[]{ 
			Pattern.compile(".*/admin-tools-core/.*"),
			Pattern.compile(".*/admin-tools-core-\\d.*")
	};
	
	private static int NEG = -1;
	private static int POS = 1;

	@Override
	public int compare(String o1, String o2) {
		boolean o1FoundInCore = foundInCore(o1);
		boolean o2FoundInCore = foundInCore(o2);
		
		if (o1FoundInCore) {
			return POS;
		}
		if (o2FoundInCore) {
			return NEG;
		}
		return o2.compareTo(o1);
	}
	
	private boolean foundInCore(String url) {
		boolean foundInCore = false;
		for (Pattern libPattern : CORE_LIB) {
			foundInCore |= libPattern.matcher(url).matches();
		}
		if (LOGGER.isTraceEnabled()) {
			LOGGER.trace(String.format("template found in core %s for %s", foundInCore, url));
		}
		return foundInCore;
	}
}
