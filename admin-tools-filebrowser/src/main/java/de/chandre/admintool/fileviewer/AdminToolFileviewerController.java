package de.chandre.admintool.fileviewer;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.controller.AbstractAdminController;
import de.chandre.admintool.filebrowser.AdminToolFilebrowserConfig;
import de.chandre.admintool.filebrowser.AdminToolFilebrowserLoader;
import de.chandre.admintool.filebrowser.AdminToolFilebrowserService;
import de.chandre.admintool.filebrowser.DownloadNotAllowedException;
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
	private AdminToolFileviewerService fileviewerService;
	
	@Autowired
	private AdminToolFilebrowserService filebrowserService;
	
	@Autowired
	private AdminToolFileviewerConfig fileviewerConfig;
	
	@Autowired
	private AdminToolFilebrowserConfig filebrowserConfig;
	
	
	@RequestMapping(value = {"/show",}, method={RequestMethod.GET, RequestMethod.POST})
	public String loadFile(@RequestParam("file") String file, @RequestParam(name="encoding", required=false) String encoding,
			ModelMap model, HttpServletRequest request) throws GenericFilebrowserException, UnsupportedEncodingException {
		if (!fileviewerConfig.isEnabled()) {
			return null;
		}
		if(LOGGER.isTraceEnabled()) LOGGER.trace("serving file viewer page for file: " + file + ", encoding: " + encoding);
		String templatePath = addCommonContextVars(model, request, "filebrowser", AdminToolFilebrowserLoader.TARGET_FILEVIEWER);
		
		String decodedPath = URLDecoder.decode(file, "UTF-8");
		File currentFile = new File(decodedPath);
		model.put("currentDir", currentFile.getParent());
		
		fileviewerService.isFileAllowed(currentFile, false);
		
		model.put("currentFile", currentFile);
		model.put("selEncoding", StringUtils.isEmpty(encoding) ? fileviewerConfig.getDefaultEncoding() : encoding);
		
		return AdminTool.ROOTCONTEXT_NAME + AdminTool.SLASH + templatePath;
	}
	
	@RequestMapping(value = {"/update",}, method={RequestMethod.POST})
	public String updateFile(@RequestParam("file") String file, @RequestParam(name="encoding", required=false) String encoding,
			@RequestParam("fileContent") String fileContent,
			ModelMap model, HttpServletRequest request) throws GenericFilebrowserException {
		if (!fileviewerConfig.isEnabled()) {
			return null;
		}
		if(LOGGER.isTraceEnabled()) LOGGER.trace("updating file: " + file + ", encoding: " + encoding);
		String templatePath = addCommonContextVars(model, request, "filebrowser", null);
		
		File currentFile = new File(file);
		model.put("currentDir", currentFile.getParent());
		
		try {
			fileviewerService.writeStringToFile(currentFile, encoding, fileContent);
		} catch (GenericFilebrowserException e) {
			throw e;
		} catch (Exception e) {
			throw new GenericFilebrowserException("Exception while writing content to file: " + e.getMessage(), e);
		}
		return AdminTool.ROOTCONTEXT_NAME + AdminTool.SLASH + templatePath;
	}
	
	@ExceptionHandler({DownloadNotAllowedException.class, GenericFilebrowserException.class})
	public ModelAndView handleException(Exception exception, HttpServletRequest request) throws IOException {
		if(LOGGER.isTraceEnabled()) LOGGER.trace("handleException: " + exception.getMessage());
		
		ModelAndView mv = new ModelAndView(AdminTool.GENERIC_ERROR_TPL_PATH);
		addCommonContextVars(mv.getModelMap(), request, "filebrowser", null);
		
		String lastFile = request.getParameter("file");
		if (StringUtils.isEmpty(lastFile)) {
			lastFile = request.getParameter("selectedFile");
		}
		String decodedPath = null;
		if (StringUtils.hasLength(lastFile)) {
			decodedPath = URLDecoder.decode(lastFile, "UTF-8");
		}
		LOGGER.info("lastFile: " + lastFile);
		if (StringUtils.hasLength(decodedPath) && 
				filebrowserService.isAllowed(new File(decodedPath).getParentFile(), false, filebrowserConfig.isReadOnly()) ) {
			mv.getModelMap().put("currentDir", new File(decodedPath).getParent());
		} else {
			mv.getModelMap().put("currentDir", filebrowserConfig.getStartDir().getAbsolutePath());
		}
		mv.getModelMap().put("exceptionMessage", exception.getMessage());
		return mv;
	}
	
}
