package de.chandre.admintool.db;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * REST controller for dbbrowser
 * @author Andre
 *
 */
@Controller
@RequestMapping("/admintool/dbbrowser")
public class AdminToolDBBrowserController 
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolDBBrowserController.class);
	
	@Autowired
	private AdminToolDBBrowserService dbBrowserService;
	
	@RequestMapping(path = "/getDatasourceNames", method = { RequestMethod.GET, RequestMethod.POST })
	@ResponseBody
	public List<String> getDatasourceNames(HttpServletRequest request) {
		if (LOGGER.isDebugEnabled()) 
			LOGGER.debug("receiving getDatasourceNames request");
		return dbBrowserService.getDatasourceNames();
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
