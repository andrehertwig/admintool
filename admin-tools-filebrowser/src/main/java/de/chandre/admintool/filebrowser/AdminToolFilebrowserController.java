package de.chandre.admintool.filebrowser;

import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import de.chandre.admintool.core.controller.AbstractAdminController;

/**
 * Filebrowser controller <br>
 * requires admintool-core 1.0.1<br>
 * @author Andre
 * @since 1.0.1
 */
@Controller
@RequestMapping(AbstractAdminController.ROOTCONTEXT + "/filebrowser")
public class AdminToolFilebrowserController extends AbstractAdminController {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolFilebrowserController.class);
	
	@Autowired
	private AdminToolFilebrowserService filebrowserService;
	
	@Autowired
	private AdminToolFilebrowserConfig filebrowserConfig;
	
	@RequestMapping(value = {"", "/","/dir"})
	public String showDirectory(@RequestParam(name = "dir", required = false) String dirPath, 
			@RequestParam(name = "sortCol", required = false) String sortCol,
			@RequestParam(name = "sortAsc", required = false, defaultValue = "true") boolean sortType,
			ModelMap model, HttpServletRequest request) {
		String currentDir = StringUtils.isEmpty(dirPath) ? filebrowserConfig.getStartDir().getAbsolutePath() : dirPath;
		if(LOGGER.isTraceEnabled()) LOGGER.trace("show directory: " + currentDir);
		addCommonContextVars(model, request, "filebrowser", null);
		model.put("currentDir", currentDir);
		model.put("sortCol", SortColumn.fromIndex(sortCol));
		model.put("sortAsc", sortType);
		return AbstractAdminController.ROOTCONTEXT_NAME + "/index";
	}
	
	@RequestMapping(value = {"/file",})
	public void showFile(@RequestParam("file") String filePath, ModelMap model, HttpServletRequest request,
			HttpServletResponse response) throws IOException, DownloadNotAllowedException, GenericFilebrowserException {
		if(LOGGER.isTraceEnabled()) LOGGER.trace("download file: " + filePath);
		filebrowserService.downloadFile(filePath, response);
	}

	@RequestMapping(value = {"/zip",})
	public void downloadAsZip(@RequestParam("selectedFile") List<String> filePaths, ModelMap model, HttpServletRequest request,
			HttpServletResponse response) throws IOException, DownloadNotAllowedException, GenericFilebrowserException {
		if(LOGGER.isTraceEnabled()) LOGGER.trace("downloadAsZip file: " + filePaths.size());
		filebrowserService.downloadFilesAsZip(filePaths, response);
	}
	
	
	@ExceptionHandler({DownloadNotAllowedException.class, GenericFilebrowserException.class})
	public String handleException(Exception ex, ModelMap model, HttpServletRequest request) {
		if(LOGGER.isTraceEnabled()) LOGGER.trace("handleException");
		addCommonContextVars(model, request, "filebrowser", null);
		model.put("exceptionMessage", ex.getMessage());
		return AbstractAdminController.ROOTCONTEXT_NAME + "/index";
	}
	
}
