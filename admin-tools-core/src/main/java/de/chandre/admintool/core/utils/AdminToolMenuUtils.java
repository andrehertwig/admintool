package de.chandre.admintool.core.utils;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.MenuEntry;

/**
 * Utils for displaying MenuEntries in a correct way
 * 
 * @author Andre
 *
 */
@Service("adminToolMenuUtils")
public class AdminToolMenuUtils {
	/**
	 * checks {@link #isActiveInMenuTree(String, MenuEntry)} and if the
	 * actualEntry has sub entries and returns the html class name
	 * 
	 * @param activeMenuName
	 *            name of the active MenuEntry (set in controller)
	 * @param actualEntry
	 *            actual iterated object
	 * @return empty string, "active", "treeview", "treeview active"
	 */
	public String getListItemClass(MenuEntry activeMenu, MenuEntry actualEntry) {
		StringBuilder sb = new StringBuilder();
		if (!CollectionUtils.isEmpty(actualEntry.getSubmenu()))
			sb.append("treeview");
		if (isActiveInMenuTree(activeMenu, actualEntry))
			sb.append(" active");
		return sb.toString().trim();
	}

	/**
	 * checks if actualEntry contains the activeMenuName in entry itself and its
	 * sub entries
	 * 
	 * @param activeMenuName
	 * @param actualEntry
	 * @return
	 */
	public boolean isActiveInMenuTree(MenuEntry activeMenu, MenuEntry actualEntry) {
		return actualEntry.flattened().anyMatch(
				entry -> checkForNull(actualEntry, activeMenu) && entry.getName().equals(activeMenu.getName()));
	}

	public String getBreadcrumb(MenuEntry actualEntry) {
		StringBuilder result = new StringBuilder();
		actualEntry.reverseFlattened().collect(toListReversed())
				.forEach(entry -> result.append(entry.getDisplayName()).append(" > "));

		String result2 = result.toString();
		result2 = result2.substring(0, result2.lastIndexOf(">"));
		return result2.trim().toString();
	}

	public static <T> Collector<T, ?, List<T>> toListReversed() {
		return Collectors.collectingAndThen(Collectors.toList(), l -> {
			Collections.reverse(l);
			return l;
		});
	}

	public boolean hasMenuEntry(AdminComponent component, MenuEntry activeMenue) {
		Optional<MenuEntry> result = component.getMainMenu().flattened()
				.filter(menu -> checkForNull(menu, activeMenue) && menu.getName().equals(activeMenue.getName()))
				.findFirst();
		return result.isPresent();
	}

	private boolean checkForNull(MenuEntry menu, MenuEntry activeMenu) {
		return null != menu.getName() && null != activeMenu && null != activeMenu.getName();
	}
}
