package de.chandre.admintool.db;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.controller.AbstractAdminController;

/**
 * REST controller for dbbrowser
 * @author Andre
 *
 */
@Controller
@RequestMapping(AdminTool.ROOTCONTEXT + "/dbbrowser")
public class AdminToolDBBrowserController extends AbstractAdminController
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolDBBrowserController.class);
	
	@Autowired
	private AdminToolDBBrowserService dbBrowserService;
	
	@Autowired
	private AdminToolDBBrowserConfig configuration;
	
	@Autowired
	private AdminToolDBBrowserExampleLoader exampleLoader;
	
	@RequestMapping(path = "/getDatasourceNames", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public List<String> getDatasourceNames(HttpServletRequest request) {
		if(!configuration.isEnabled()) return null;
		
		if (LOGGER.isDebugEnabled()) 
			LOGGER.debug("receiving getDatasourceNames request");
		return dbBrowserService.getDatasourceNames();
	}
	
	@RequestMapping(path = "/getMetaData/{datasourceName}", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public QueryResultTO getMetaData(@PathVariable("datasourceName") String datasourceName, HttpServletRequest request) {
		if(!configuration.isEnabled()) return null;
		
		if (LOGGER.isDebugEnabled()) 
			LOGGER.debug("receiving getMetaData request");
		return dbBrowserService.getMetadata(datasourceName);
	}
	
	@RequestMapping(path = "/getExamples", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public String getExamples(HttpServletRequest request) {
		if(!configuration.isEnabled()) return null;
		
		if (LOGGER.isDebugEnabled()) 
			LOGGER.debug("receiving getDatasourceNames request");
		return new JSONObject(exampleLoader.getExamples()).toString();
	}
	
	@RequestMapping(path = "/executeQuery", method = {RequestMethod.POST })
	public String executeQuery(@RequestBody StatementTO statementTO, ModelMap model, HttpServletRequest request) {
		if(!configuration.isEnabled()) return null;
		
		if (LOGGER.isDebugEnabled()) 
			LOGGER.debug("receiving executeQuery request");
		if (LOGGER.isTraceEnabled())
			LOGGER.trace("with object: " + statementTO.toString());
		
		model.put("statementTO", statementTO);
		model.put("queryResultTO", dbBrowserService.queryDatabase(statementTO));
		return AdminTool.ROOTCONTEXT_NAME + "/dbbrowser/includes/tabInclude";
	}

	/**
	 * @return the dbBrowserService
	 */
	public AdminToolDBBrowserService getDbBrowserService() {
		return dbBrowserService;
	}

	/**
	 * if you want to override some functionality, just inherit from {@link AdminToolDBBrowserServiceImpl},
	 *  override your methods and call this setter
	 * @param dbBrowserService the dbBrowserService to set
	 */
	public void setDbBrowserService(AdminToolDBBrowserService dbBrowserService) {
		this.dbBrowserService = dbBrowserService;
	}
}
