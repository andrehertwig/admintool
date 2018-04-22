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
import de.chandre.admintool.core.controller.AbstractAdminController;
import de.chandre.admintool.core.ui.ATError;
import de.chandre.admintool.core.ui.select2.Select2GroupedTO;
import de.chandre.admintool.security.dbuser.auth.AccessRelationTO;
import de.chandre.admintool.security.dbuser.domain.ATClient;
import de.chandre.admintool.security.dbuser.service.AdminToolSecDBClientService;

/**
 * Client controller
 * @author André
 * @since 1.1.7
 *
 */
@Controller
@RequestMapping(AdminTool.ROOTCONTEXT + "/accessmanagement/client")
public class AdminToolSecDBClientController extends AbstractAdminController {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolSecDBClientController.class);

	@Autowired
	private AdminToolSecDBClientService clientService;
	
	@Autowired
	private AdminToolSecDBTransformUtil transformUtil;
	
	@RequestMapping(path="/get", method=RequestMethod.GET)
	@ResponseBody
	public Select2GroupedTO<?> getUserGroups() {
		List<ATClient> clients = clientService.getAllClients();
		return transformUtil.transformAccessRelationToSelect2(clients);
	}
	
	@RequestMapping(path="/state/name/{name}", method=RequestMethod.POST)
	@ResponseBody
	public String changeState(@PathVariable("name") String name) {
		try {
			if (null != clientService.changeState(name)) {
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
		return clientService.updateClient(accessRelationTO);
	}
	
	@RequestMapping(path="/add", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> add(@RequestBody AccessRelationTO accessRelationTO) {
		return clientService.addClient(accessRelationTO);
	}
	
	@RequestMapping(path="/remove/name/{name}", method=RequestMethod.POST)
	@ResponseBody
	public String remove(@PathVariable("name") String name) {
		try {
			clientService.removeByName(name);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return Boolean.FALSE.toString();
		}
		return Boolean.TRUE.toString();
	}
}
