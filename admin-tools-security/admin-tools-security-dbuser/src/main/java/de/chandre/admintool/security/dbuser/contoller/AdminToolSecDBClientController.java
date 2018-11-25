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
import de.chandre.admintool.security.dbuser.domain.ATClient;
import de.chandre.admintool.security.dbuser.service.AdminToolSecDBClientService;
import de.chandre.admintool.security.dbuser.service.validation.AdminToolSecDBClientValidator;

/**
 * Client controller
 * @author André
 * @since 1.2.0
 *
 */
@Controller
@RequestMapping(AdminTool.ROOTCONTEXT + "/accessmanagement/client")
public class AdminToolSecDBClientController extends ATSecDBAbctractController {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolSecDBClientController.class);

	@Autowired
	private AdminToolSecDBClientService clientService;
	
	@Autowired
	private AdminToolSecDBTransformUtil transformUtil;
	
	@Autowired
	private AdminToolSecDBClientValidator validator;
	
	@RequestMapping(path="/get", method=RequestMethod.GET)
	@ResponseBody
	public Select2GroupedTO<?> getClients() {
		try {
			List<ATClient> clients = clientService.getAllClients();
			return transformUtil.transformAccessRelationToSelect2(clients);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			Select2GroupedTO<OptionTO> to = new Select2GroupedTO<>();
			to.setErrors(super.handleException(e, LOGGER, this.validator, "error.get.client", "get.client.error", "Could not get list of clients"));
			return to;
		}
	}
	
	@RequestMapping(path="/state/name/{name}", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> changeState(@PathVariable("name") String name) {
		try {
			if (null != clientService.changeState(name)) {
				return Collections.emptySet();
			}
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return super.handleException(e, LOGGER, validator, "error.update.state.client", "update.state.client.error", "Could not  not change Client state");
		}
		return super.createError(this.validator, "error.update.state.client", "update.state.client.error", "Could not change client state for client: ", name);
	}
	
	@RequestMapping(path="/update", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> update(@RequestBody AccessRelationTO accessRelationTO) {
		try {
			return clientService.updateClient(accessRelationTO);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return super.handleException(e, LOGGER, validator, "error.update.state.client", "update.client.error", "Could not update Client");
		}
	}
	
	@RequestMapping(path="/add", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> add(@RequestBody AccessRelationTO accessRelationTO) {
		try {
			return clientService.addClient(accessRelationTO);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return super.handleException(e, LOGGER, validator, "error.add.client", "add.client.error", "Could not add Client");
		}
	}
	
	@RequestMapping(path="/remove/name/{name}", method=RequestMethod.POST)
	@ResponseBody
	public Set<ATError> remove(@PathVariable("name") String name) {
		try {
			clientService.removeByName(name);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return super.handleException(e, LOGGER, validator, "error.remove.client", "remove.client.error", "Could not remove Client", name);
		}
		return Collections.emptySet();
	}
}
