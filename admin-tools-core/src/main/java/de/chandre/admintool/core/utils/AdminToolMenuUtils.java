package de.chandre.admintool.core.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import de.chandre.admintool.core.AdminTool;
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
	
	@Autowired
	private AdminTool adminTool;
	
	public List<AdminComponent> getComponents() {
		List<AdminComponent> result = new ArrayList<>();
		
		for (AdminComponent adminComponent : adminTool.getComponents()) {
			Stream<MenuEntry> nonHiddenMenues = adminComponent.getMainMenu().flattened().filter(me -> !me.isHide());
			if (nonHiddenMenues.count() == 0L) {
				continue;
			}
			result.add(adminComponent);
		}
		
		return result;
	}
	
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

	/**
	 * return
	 * @param actualEntry
	 * @param separator 
	 * @return
	 */
	public String getBreadcrumb(MenuEntry actualEntry, String separator) {
		StringBuilder result = new StringBuilder();
		final String sep;
		if (StringUtils.isEmpty(separator)) {
			sep =  " > ";
		} else {
			sep = separator;
		}
		actualEntry.reverseFlattened().collect(toListReversed())
				.forEach(entry -> result.append(entry.getDisplayName()).append(sep));

		String result2 = result.toString();
		result2 = result2.substring(0, result2.lastIndexOf(sep));
		return result2.trim().toString();
	}
	
	/**
	 * returns a linked list of reverse resolution o menu structure
	 * @param actualEntry
	 * @return
	 */
	public List<MenuEntry> getBreadcrumbList(MenuEntry actualEntry) {
		List<MenuEntry> result = new LinkedList<>();
		if (null != actualEntry) {
			actualEntry.reverseFlattened().collect(toListReversed()).forEach(entry -> {
				if(null != entry) result.add(entry);
			});
		}
		return result;
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
