package de.chandre.admintool.security.dbuser.contoller;

import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.core.ui.select2.Select2GroupedTO;
import de.chandre.admintool.security.dbuser.auth.AccessRelationTO;
import de.chandre.admintool.security.dbuser.domain.ATUserGroup;
import de.chandre.admintool.security.dbuser.service.AdminToolSecDBUserGroupService;
import de.chandre.admintool.security.dbuser.service.validation.AdminToolSecDBUserGroupValidator;

/**
 * UserGroup Controller
 * @author André
 * @since 1.1.7
 *
 */
@Controller
@RequestMapping(AdminTool.ROOTCONTEXT + "/accessmanagement/usergroup")
public class AdminToolSecDBUserGroupController extends ATSecDBAbctractController {
	
	private final Log LOGGER = LogFactory.getLog(AdminToolSecDBUserGroupController.class);

	@Autowired
	private AdminToolSecDBUserGroupService userGroupService;
	
	@Autowired
	private AdminToolSecDBTransformUtil transformUtil;
	
	@Autowired
	private AdminToolSecDBUserGroupValidator validator;
	
	@RequestMapping(path="/get", method=RequestMethod.GET)
	@ResponseBody
	public Select2GroupedTO<?> getUserGroups() {
		List<ATUserGroup> usergroups = userGroupService.getAllUserGroups();
		return transformUtil.transformAccessRelationToSelect2(usergroups);
	}
	
	@RequestMapping(path="/state/name/{name}", method=RequestMethod.POST)
	@ResponseBody
	public String changeState(@PathVariable("name") String name) {
		try {
			if (null != userGroupService.changeState(name)) {
				return Boolean.TRUE.toString();
			}
		} catch (Exception e) {
			LOGGER.debug(e.getMessage(), e);
		}
		return Boolean.FALSE.toString();
	}
	
	@RequestMapping(path="/update", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> update(@RequestBody AccessRelationTO accessRelationTO) {
		try {
			return userGroupService.updateUserGroup(accessRelationTO);
		} catch (Exception e) {
			return handleException(e, LOGGER, validator, "error.update.group", "update.group.error", "Could not update UserGroup");
		}
	}
	
	@RequestMapping(path="/add", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> add(@RequestBody AccessRelationTO accessRelationTO) {
		try {
			return userGroupService.addUserGroup(accessRelationTO);
		} catch (Exception e) {
			return handleException(e, LOGGER, validator, "error.add.group", "add.group.error", "Could not add UserGroup");
		}
	}
	
	@RequestMapping(path="/remove/name/{name}", method=RequestMethod.POST)
	@ResponseBody
	public String remove(@PathVariable("name") String name) {
		try {
			userGroupService.removeByName(name);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return Boolean.FALSE.toString();
		}
		return Boolean.TRUE.toString();
	}
}
