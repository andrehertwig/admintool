package de.chandre.admintool.core.thymeleaf;

import java.util.Comparator;

/**
 * Comparator expects the full url path of template<br>
 * Templates found in core will have the lowest order
 * @author Andre
 * @since 1.0.1
 *
 */
public class TemplateUrlComparator implements Comparator<String> {
	
	private static final String[] CORE_LIB = new String[]{"/admin-tools-core/", "/admin-tools-core-"};
	
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
		for (String lib : CORE_LIB) {
			foundInCore |= url.contains(lib);
		}
		return foundInCore;
	}

}
