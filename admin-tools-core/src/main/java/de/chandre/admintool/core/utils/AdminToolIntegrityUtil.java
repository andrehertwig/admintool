package de.chandre.admintool.core.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.AdminToolCoreConfig;
import de.chandre.admintool.core.component.AdminComponent;
import de.chandre.admintool.core.component.MenuEntry;

/**
 * Util for integrity check for components and menu entries
 * 
 * @author Andre
 *
 */
@Service("adminToolIntegrityUtil")
public class AdminToolIntegrityUtil {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolIntegrityUtil.class);
	
	public static final String MSG_KEY_DUPLICATE_LINK = "admintool.integrity.error.duplicate.link";
	public static final String MSG_KEY_DUPLICATE_TPL_REF = "admintool.integrity.error.duplicate.templateReference";
	public static final String MSG_KEY_COMPONENT_NO_MAINMENU = "admintool.integrity.error.component.missingMainMenu";
	public static final String MSG_KEY_MISSING_RESOURCE_KEY = "admintool.integrity.error.missing.resourceKey";
	
	@Autowired
	private AdminTool adminTool;
	
	@Autowired
	private AdminToolMenuUtils menuUtils;
	
	@Autowired
	private AdminToolCoreConfig config;

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

		Map<String, MenuEntry> links = new HashMap<>();
		Map<String, MenuEntry> templates = new HashMap<>();

		// check for duplicates, but only if menu has no submenu, otherwise o
		// link will be generated
		for (AdminComponent comp : adminTool.getComponents()) {
			if (null != comp.getMainMenu()) {
				comp.getMainMenu().flattened().forEach(menu -> {
					if (links.containsKey(menu.getName()) && CollectionUtils.isEmpty(menu.getSubmenu())) {
						findErrorAndAddEntry("duplicate link name on menu item", 
								MSG_KEY_DUPLICATE_LINK, errorList, menu, links.get(menu.getName()));
					} else {
						links.put(menu.getName(), menu);
					}
					if (templates.containsKey(menu.getTarget()) && CollectionUtils.isEmpty(menu.getSubmenu())) {
						findErrorAndAddEntry("duplicate template reference on menu item", 
								MSG_KEY_DUPLICATE_TPL_REF, errorList, menu, templates.get(menu.getTarget()));
					} else {
						templates.put(menu.getTarget(), menu);
					}
					if(config.isInternationalizationEnabled() && StringUtils.isEmpty(menu.getResouceMessageKey())) {
						findErrorAndAddEntry("missing message resource key on menu item", 
								MSG_KEY_MISSING_RESOURCE_KEY, errorList, menu, templates.get(menu.getTarget()));
					}
					
				});
			} else {
				findErrorAndAddEntry(String.format("the component '%s' has no main menu", comp.getDisplayName()), 
						MSG_KEY_COMPONENT_NO_MAINMENU, errorList, null, null);
			}
		}
		links.clear();
		templates.clear();
		return errorList;
	}
	
	public void checkMenuIntegrityAndPrintLog() {
		List<MenuIntegrityError> errors = checkMenuIntegrity();
		String result = "successful";
		if (!CollectionUtils.isEmpty(errors)) {
			result = "failure";
		}
		LOGGER.info(String.format("------ Menu integrity check: %s", result));
		
		if (!CollectionUtils.isEmpty(errors)) {
			errors.forEach(menuIntegrityError ->{
				LOGGER.warn(String.format("  %s. Occured %s times.", 
						menuIntegrityError.getError(), menuIntegrityError.getMenuEntries().size()));
				menuIntegrityError.getMenuEntries().forEach(menu -> {
					LOGGER.warn(String.format("     - %s seems to be equals to %s", 
							menuUtils.getBreadcrumb(menu, "->"), menuUtils.getBreadcrumb(menuIntegrityError.getReference(), "->")));
				});
			});
		}
	}

	private void findErrorAndAddEntry(String clusterName, String errorMsgKey, List<MenuIntegrityError> errorList, 
			MenuEntry menuEntry, MenuEntry reference) {
		Optional<MenuIntegrityError> errorRes = errorList.stream().filter(error -> error.getError().equals(clusterName))
				.findFirst();
		MenuIntegrityError intError = null;
		if (!errorRes.isPresent()) {
			intError = new MenuIntegrityError(clusterName, errorMsgKey, reference);
			errorList.add(intError);
		} else {
			intError = errorRes.get();
		}
		intError.addMenuEntry(menuEntry);
	}
}
