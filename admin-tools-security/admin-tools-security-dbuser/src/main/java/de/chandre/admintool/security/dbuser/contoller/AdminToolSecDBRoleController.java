package de.chandre.admintool.security.dbuser.contoller;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.CollectionUtils;
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
import de.chandre.admintool.security.dbuser.service.AdminToolSecDBRoleService;
import de.chandre.admintool.security.dbuser.service.AdminToolSecDBUserGroupService;
import de.chandre.admintool.security.dbuser.service.validation.AdminToolSecDBRoleValidator;

/**
 * Role controller
 * @author André
 * @since 1.2.0
 *
 */
@Controller
@RequestMapping(AdminTool.ROOTCONTEXT + "/accessmanagement/role")
public class AdminToolSecDBRoleController extends ATSecDBAbctractController {
	
	private final Log LOGGER = LogFactory.getLog(AdminToolSecDBRoleController.class);
	
	@Autowired
	private AdminToolSecDBRoleService roleService;
	
	@Autowired
	private AdminToolSecDBUserGroupService userGroupService;
	
	@Autowired
	private AdminToolSecDBTransformUtil transformUtil;
	
	@Autowired
	private AdminToolSecDBRoleValidator validator;

	@RequestMapping(path="/get", method=RequestMethod.GET)
	@ResponseBody
	public Select2GroupedTO<?> getUserGroups() {
		try {
			List<ATRole> roles = roleService.getAllRoles();
			return transformUtil.transformAccessRelationToSelect2(roles);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			Select2GroupedTO<OptionTO> to = new Select2GroupedTO<>();
			to.setErrors(super.handleException(e, LOGGER, this.validator, "error.get.role", "get.role.error", "Could not get list of roles"));
			return to;
		}
	}
	
	@RequestMapping(path="/{name}/usergroups", method=RequestMethod.GET)
	@ResponseBody
	public List<AccessRelationTO> getUserGroupsByRoleName(@PathVariable("name") String roleName ) {
		List<ATUserGroup> userGroups = userGroupService.getUserGroupsByRoleName(roleName);
		if (!CollectionUtils.isEmpty(userGroups)) {
			return userGroups.stream().map(ug -> {
				AccessRelationTO ugTo = new AccessRelationTO();
				ugTo.setActive(ug.isActive());
				ugTo.setName(ug.getName());
				ugTo.setDisplayName(ug.getDisplayName());
				ugTo.setDescription(ug.getDescription());
				return ugTo;
			}).collect(Collectors.toList());
		}
		return Collections.emptyList();
	}
	
	@RequestMapping(path="/state/name/{name}", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> changeState(@PathVariable("name") String name) {
		try {
			if (null != roleService.changeState(name)) {
				return Collections.emptySet();
			}
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return handleException(e, LOGGER, validator, "error.update.state.role", "update.state.role.error", "Could not update Role state");
		}
		return createError(validator,  "error.update.state.role", "update.state.role.error", "Could not update Role state for role: ", name);
	}
	
	@RequestMapping(path="/update", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> update(@RequestBody AccessRelationTO accessRelationTO) {
		try {
			return roleService.updateRole(accessRelationTO);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return handleException(e, LOGGER, validator, "error.update.role", "update.role.error", "Could not update Role");
		}
	}
	
}
