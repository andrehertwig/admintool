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
import de.chandre.admintool.security.dbuser.domain.ATRole;
import de.chandre.admintool.security.dbuser.service.AdminToolSecDBRoleService;
import de.chandre.admintool.security.dbuser.service.validation.AdminToolSecDBRoleValidator;

/**
 * Role controller
 * @author André
 * @since 1.1.7
 *
 */
@Controller
@RequestMapping(AdminTool.ROOTCONTEXT + "/accessmanagement/role")
public class AdminToolSecDBRoleController extends ATSecDBAbctractController {
	
	private final Log LOGGER = LogFactory.getLog(AdminToolSecDBRoleController.class);
	
	@Autowired
	private AdminToolSecDBRoleService roleService;
	
	@Autowired
	private AdminToolSecDBTransformUtil transformUtil;
	
	@Autowired
	private AdminToolSecDBRoleValidator validator;

	@RequestMapping(path="/get", method=RequestMethod.GET)
	@ResponseBody
	public Select2GroupedTO<?> getUserGroups() {
		List<ATRole> roles = roleService.getAllRoles();
		return transformUtil.transformAccessRelationToSelect2(roles);
	}
	
	@RequestMapping(path="/state/name/{name}", method=RequestMethod.POST)
	@ResponseBody
	public String changeState(@PathVariable("name") String name) {
		try {
			if (null != roleService.changeState(name)) {
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
			return roleService.updateRole(accessRelationTO);
		} catch (Exception e) {
			return handleException(e, LOGGER, validator, "error.update.role", "update.role.error", "Could not update Role");
		}
		
	}
	
}
