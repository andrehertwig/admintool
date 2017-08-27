package de.chandre.admintool.jminix;

import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.controller.AbstractAdminController;
import de.chandre.admintool.jmx.jstree.JmxExecuteTO;
import de.chandre.admintool.jmx.jstree.JmxQueryTO;
import de.chandre.admintool.jmx.jstree.JmxResponseTO;
import de.chandre.admintool.jmx.jstree.JsTree;

@Controller
@RequestMapping(AdminTool.ROOTCONTEXT + "/jmx")
public class AdminToolJmxController extends AbstractAdminController {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolJmxController.class);

	@Autowired
	private AdminToolJmxService jmxService;
	
	@RequestMapping(value = {"/tree"}, method=RequestMethod.GET, produces={MediaType.APPLICATION_JSON_UTF8_VALUE})
	@ResponseBody
	public List<JsTree> getNode(HttpServletRequest request, HttpServletResponse response) throws IOException {
		LOGGER.debug("building JMX tree");
		try {
			return jmxService.buildTree();
		} catch (Exception e) {
			e.printStackTrace();
			response.sendError(500, e.getMessage());
		}
		return null;
	}
	
	@RequestMapping(value = {"/attributes", "/attribute"}, method=RequestMethod.POST, produces={MediaType.APPLICATION_JSON_UTF8_VALUE})
	@ResponseBody
	public JmxResponseTO attributes(@RequestBody JmxQueryTO queryTO, HttpServletRequest request, HttpServletResponse response) throws IOException {
		LOGGER.debug("resolving attributes: " + queryTO);
		try {
			return jmxService.getAttributes(queryTO);
		} catch (Exception e) {
			e.printStackTrace();
			response.sendError(500, e.getMessage());
		}
		return null;
	}
	
	@RequestMapping(value = {"/operation"}, method=RequestMethod.POST, produces={MediaType.APPLICATION_JSON_UTF8_VALUE})
	@ResponseBody
	public JmxResponseTO operations(@RequestBody JmxQueryTO queryTO, HttpServletRequest request, HttpServletResponse response) throws IOException {
		LOGGER.debug("resolving operations: " + queryTO);
		try {
			return jmxService.getOpperation(queryTO);
		} catch (Exception e) {
			e.printStackTrace();
			response.sendError(500, e.getMessage());
		}
		return null;
	}
	
	@RequestMapping(value = {"/operation/execute"}, method=RequestMethod.POST, produces={MediaType.APPLICATION_JSON_UTF8_VALUE})
	@ResponseBody
	public JmxResponseTO updateValue(@RequestBody JmxExecuteTO executeTO, HttpServletRequest request, HttpServletResponse response) throws IOException {
		LOGGER.debug("update value: " + executeTO);
		try {
			return jmxService.getExecuteOperation(executeTO);
		} catch (Exception e) {
			e.printStackTrace();
			response.sendError(500, e.getMessage());
		}
		return null;
	}
}
