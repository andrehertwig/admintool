package de.chandre.admintool.filebrowser;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.ArrayList;
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
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.core.controller.AbstractAdminController;

/**
 * Filebrowser controller <br>
 * requires admintool-core 1.0.1<br>
 * @author Andre
 * @since 1.0.1
 */
@Controller
@RequestMapping(AdminTool.ROOTCONTEXT + "/filebrowser")
public class AdminToolFilebrowserController extends AbstractAdminController {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolFilebrowserController.class);
	
	@Autowired
	private AdminToolFilebrowserService filebrowserService;
	
	@Autowired
	private AdminToolFilebrowserConfig filebrowserConfig;
	
	@RequestMapping(value = {"", "/","/dir"}, method={RequestMethod.GET, RequestMethod.POST})
	public String showDirectory(@RequestParam(name = "dir", required = false) String dirPath, 
			@RequestParam(name = "sortCol", required = false) String sortCol,
			@RequestParam(name = "sortAsc", required = false, defaultValue = "true") boolean sortType,
			@RequestParam(name = "filter", required = false) String filter,
			ModelMap model, HttpServletRequest request) throws UnsupportedEncodingException {
		String currentDir = StringUtils.isEmpty(dirPath) ? filebrowserConfig.getStartDir().getAbsolutePath() : URLDecoder.decode(dirPath, "UTF-8");
		if(LOGGER.isTraceEnabled()) LOGGER.trace("show directory: " + currentDir);
		String templatePath = addCommonContextVars(model, request, "filebrowser", null);
		model.put("currentDir", currentDir);
		model.put("sortCol", SortColumn.fromIndex(sortCol));
		model.put("sortAsc", sortType);
		model.put("filter", filter);
		return AdminTool.ROOTCONTEXT_NAME + AdminTool.SLASH + templatePath;
	}
	
	@RequestMapping(value = {"/file"}, method={RequestMethod.GET, RequestMethod.POST})
	public void showFile(@RequestParam("file") String filePath, ModelMap model, HttpServletRequest request,
			HttpServletResponse response) throws IOException, DownloadNotAllowedException, GenericFilebrowserException {
		if (!filebrowserConfig.isDownloadAllowed()) {
			throw new DownloadNotAllowedException("file download is deactivated by configuration");
		}
		String decodedPath = URLDecoder.decode(filePath, "UTF-8");
		if(LOGGER.isTraceEnabled()) LOGGER.trace("download file: " + decodedPath);
		filebrowserService.downloadFile(decodedPath, response, false);
	}
	
	@RequestMapping(value = {"/download"}, method={RequestMethod.GET, RequestMethod.POST})
	public void download(@RequestParam("file") String filePath, ModelMap model, HttpServletRequest request,
			HttpServletResponse response) throws IOException, DownloadNotAllowedException, GenericFilebrowserException {
		if (!filebrowserConfig.isDownloadAllowed()) {
			throw new DownloadNotAllowedException("file download is deactivated by configuration");
		}
		String decodedPath = URLDecoder.decode(filePath, "UTF-8");
		if(LOGGER.isTraceEnabled()) LOGGER.trace("download file: " + decodedPath);
		filebrowserService.downloadFile(decodedPath, response, true);
	}

	@RequestMapping(value = {"/zip"}, method={RequestMethod.GET, RequestMethod.POST})
	public void downloadAsZip(@RequestParam("selectedFile") List<String> filePaths, ModelMap model, HttpServletRequest request,
			HttpServletResponse response) throws IOException, DownloadNotAllowedException, GenericFilebrowserException {
		if (!filebrowserConfig.isDownloadAllowed()) {
			throw new DownloadNotAllowedException("file download is deactivated by configuration");
		}
		List<String> decodedPaths = new ArrayList<>();
		if (null != filePaths) {
			filePaths.forEach(filePath ->{
				try {
					decodedPaths.add(URLDecoder.decode(filePath, "UTF-8"));
				} catch (UnsupportedEncodingException e) {
					LOGGER.error(e.getMessage(), e);
				}
			});
		}
		
		if(LOGGER.isTraceEnabled()) LOGGER.trace("downloadAsZip file: " + decodedPaths.size());
		filebrowserService.downloadFilesAsZip(decodedPaths, response);
	}
	
	@ExceptionHandler({DownloadNotAllowedException.class, GenericFilebrowserException.class})
	public ModelAndView handleException(Exception exception, HttpServletRequest request) {
		if(LOGGER.isTraceEnabled()) LOGGER.trace("handleException: " + exception.getMessage());
		
		ModelAndView mv = new ModelAndView(AdminTool.GENERIC_ERROR_TPL_PATH);
		addCommonContextVars(mv.getModelMap(), request, "filebrowser", null);
		
		String lastFile = request.getParameter("file");
		if (StringUtils.isEmpty(lastFile)) {
			lastFile = request.getParameter("selectedFile");
		}
		LOGGER.info("lastFile: " + lastFile);
		mv.getModelMap().put("currentDir", StringUtils.hasLength(lastFile) ? 
				new File(lastFile).getParent() : filebrowserConfig.getStartDir().getAbsolutePath());
		mv.getModelMap().put("exceptionMessage", exception.getMessage());
		return mv;
	}
	
}
