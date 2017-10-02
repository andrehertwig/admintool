package de.chandre.admintool.core.component;

import java.util.Comparator;

/**
 * default comparator or ordering components against its position or its display name
 * @author Andre
 * @since 1.0.1
 */
public class AdminComponentComparator implements Comparator<AdminComponent> {
	
	private static int NEG = -1;
	private static int POS = 1;

	@Override
	public int compare(AdminComponent o1, AdminComponent o2) {
		
		if (null == o1.getPosition() && null == o2.getPosition()) {
			return o1.compareTo(o2);
		}
		else if (null == o1.getPosition()) {
			return POS;
		}
		else if (null == o2.getPosition()) {
			return NEG;
		}
		else if (o1.getPosition().intValue() == o2.getPosition().intValue()) {
			return o1.compareTo(o2);
		}
		return o1.getPosition().compareTo(o2.getPosition());
	}
}
