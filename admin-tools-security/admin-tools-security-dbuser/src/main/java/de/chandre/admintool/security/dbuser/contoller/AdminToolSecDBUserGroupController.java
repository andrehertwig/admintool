package de.chandre.admintool.security.dbuser.contoller;

import java.util.Collections;
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
import de.chandre.admintool.core.ui.select2.OptionTO;
import de.chandre.admintool.core.ui.select2.Select2GroupedTO;
import de.chandre.admintool.security.dbuser.auth.AccessRelationTO;
import de.chandre.admintool.security.dbuser.domain.ATRole;
import de.chandre.admintool.security.dbuser.domain.ATUserGroup;
import de.chandre.admintool.security.dbuser.service.AdminToolSecDBUserGroupService;
import de.chandre.admintool.security.dbuser.service.validation.AdminToolSecDBUserGroupValidator;

/**
 * UserGroup Controller
 * @author André
 * @since 1.2.0
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
		try {
			List<ATUserGroup> usergroups = userGroupService.getAllUserGroups();
			return transformUtil.transformAccessRelationToSelect2(usergroups);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			Select2GroupedTO<OptionTO> to = new Select2GroupedTO<>();
			to.setErrors(super.handleException(e, LOGGER, this.validator, "error.get.group", "get.group.error", "Could not get list of groups"));
			return to;
		}
	}
	
	@RequestMapping(path="/state/name/{name}", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> changeState(@PathVariable("name") String name) {
		try {
			if (null != userGroupService.changeState(name)) {
				return Collections.emptySet();
			}
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return handleException(e, LOGGER, validator, "error.update.state.group", "update.state.group.error", "Could not change UserGroup state");
		}
		return super.createError(this.validator, "error.update.state.group", "update.state.group.error", "Could not change UserGroup state for group: ", name);
	}
	
	@RequestMapping(path="/update", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> update(@RequestBody AccessRelationTO accessRelationTO) {
		try {
			return userGroupService.updateUserGroup(accessRelationTO);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return handleException(e, LOGGER, validator, "error.update.group", "update.group.error", "Could not update UserGroup");
		}
	}
	
	@RequestMapping(path="/add", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> add(@RequestBody AccessRelationTO accessRelationTO) {
		try {
			return userGroupService.addUserGroup(accessRelationTO);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return handleException(e, LOGGER, validator, "error.add.group", "add.group.error", "Could not add UserGroup");
		}
	}
	
	@RequestMapping(path="/remove/name/{name}", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> remove(@PathVariable("name") String name) {
		try {
			userGroupService.removeByName(name);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return handleException(e, LOGGER, validator, "error.remove.group", "remove.group.error", "Could not remove UserGroup");
		}
		return Collections.emptySet();
	}
}
