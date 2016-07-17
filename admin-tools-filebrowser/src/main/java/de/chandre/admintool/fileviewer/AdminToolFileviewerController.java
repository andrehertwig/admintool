package de.chandre.admintool.fileviewer;

import java.io.File;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.controller.AbstractAdminController;
import de.chandre.admintool.filebrowser.AdminToolFilebrowserLoader;
import de.chandre.admintool.filebrowser.GenericFilebrowserException;

/**
 * Fileviewer controller <br>
 * requires admintool-core 1.0.1<br>
 * @author Andre
 * @since 1.0.1
 */
@Controller
@RequestMapping(AdminTool.ROOTCONTEXT + "/fileviewer")
public class AdminToolFileviewerController extends AbstractAdminController {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolFileviewerController.class);
	
	@Autowired
	private AdminToolFileviewerService filebrowserService;
	
	@Autowired
	private AdminToolFileviewerConfig fileviewerConfig;
	
	
	@RequestMapping(value = {"/show",}, method={RequestMethod.GET, RequestMethod.POST})
	public String loadFile(@RequestParam("file") String file, @RequestParam(name="encoding", required=false) String encoding,
			ModelMap model, HttpServletRequest request) throws GenericFilebrowserException {
		
		if(LOGGER.isTraceEnabled()) LOGGER.trace("serving file viewer page for file: " + file + ", encoding: " + encoding);
		addCommonContextVars(model, request, "filebrowser", AdminToolFilebrowserLoader.TARGET_FILEVIEWER);
		
		File currentFile = new File(file);
		model.put("currentDir", currentFile.getParent());
		
		filebrowserService.isFileAllowed(currentFile, false);
		
		model.put("currentFile", currentFile);
		model.put("selEncoding", StringUtils.isEmpty(encoding) ? fileviewerConfig.getDefaultEncoding() : encoding);
		
		return AdminTool.ROOTCONTEXT_NAME + "/index";
	}
	
	@RequestMapping(value = {"/update",}, method={RequestMethod.POST})
	public String updateFile(@RequestParam("file") String file, @RequestParam(name="encoding", required=false) String encoding,
			@RequestParam("fileContent") String fileContent,
			ModelMap model, HttpServletRequest request) throws GenericFilebrowserException {
		
		if(LOGGER.isTraceEnabled()) LOGGER.trace("updating file: " + file + ", encoding: " + encoding);
		addCommonContextVars(model, request, "filebrowser", null);
		
		File currentFile = new File(file);
		model.put("currentDir", currentFile.getParent());
		
		filebrowserService.writeStringToFile(currentFile, encoding, fileContent);
		
		return AdminTool.ROOTCONTEXT_NAME + "/index";
	}
	
}
