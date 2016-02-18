package de.chandre.admintool.core.utils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.MenuEntry;

/**
 * Util for integrity check for some componenets
 * 
 * @author Andre
 *
 */
@Service("adminToolIntegrityUtil")
public class AdminToolIntegrityUtil {
	@Autowired
	private AdminTool adminTool;

	/**
	 * checks for integrity errors
	 * 
	 * @return true if errors are present
	 */
	public boolean hasMenuIntegrityErrors() {
		return !CollectionUtils.isEmpty(checkMenuIntegrity());
	}

	/**
	 * checks for integrity errors
	 * 
	 * @return empty list or errors
	 */
	public List<MenuIntegrityError> checkMenuIntegrity() {
		List<MenuIntegrityError> errorList = new ArrayList<>();

		Set<String> links = new HashSet<>();
		Set<String> templates = new HashSet<>();

		// check for duplicates, but only if menu has no submenu, otherwise o
		// link will be generated
		for (AdminComponent comp : adminTool.getComponents()) {
			comp.getMainMenu().flattened().forEach(menu -> {
				if (links.contains(menu.getName()) && CollectionUtils.isEmpty(menu.getSubmenu())) {
					findErrorAndAddEntry("duplicate link name", errorList, menu);
				} else {
					links.add(menu.getName());
				}
				if (templates.contains(menu.getTarget()) && CollectionUtils.isEmpty(menu.getSubmenu())) {
					findErrorAndAddEntry("duplicate template reference", errorList, menu);
				} else {
					templates.add(menu.getTarget());
				}
			});
		}
		links.clear();
		templates.clear();
		return errorList;
	}

	private void findErrorAndAddEntry(String clusterName, List<MenuIntegrityError> errorList, MenuEntry menuEntry) {
		Optional<MenuIntegrityError> errorRes = errorList.stream().filter(error -> error.getError().equals(clusterName))
				.findFirst();
		MenuIntegrityError intError = null;
		if (!errorRes.isPresent()) {
			intError = new MenuIntegrityError(clusterName);
			errorList.add(intError);
		} else {
			intError = errorRes.get();
		}
		intError.addMenuEntry(menuEntry);
	}
}
