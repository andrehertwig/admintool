package de.chandre.admintool.fileviewer;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import de.chandre.admintool.filebrowser.AbstractFileBrowserService;
import de.chandre.admintool.filebrowser.GenericFilebrowserException;

/**
 * 
 * @author Andre
 * @since 1.0.1
 */
@Service("adminToolFileviewerService")
public class AdminToolFileviewerServiceImpl extends AbstractFileBrowserService implements AdminToolFileviewerService {
	
	@Autowired
	private AdminToolFileviewerConfig config;
	
	@Autowired(required=false)
	private ATFileviewerPermissionHandler permissionHandler;
	
	@Override
	public void isFileAllowed(File file, boolean write) throws GenericFilebrowserException {
		
		if (null != permissionHandler) {
			if (!permissionHandler.isAllowed(file, write)) {
				throw new GenericFilebrowserException("insufficient file permissions");
			}
		}
		
		try {
			if (!config.isEnabled() || !isAllowed(file, write, config.isReadOnly()) || !isExtensionAllowedAndReadable(file)
					|| (write && !isExtensionAllowedAndWriteable(file))) {
				throw new GenericFilebrowserException("insufficient file permissions");
			}
		} catch (IOException e) {
			throw new GenericFilebrowserException("Error while try to check file permission: " + e.getMessage(), e);
		}
	}
	
	@Override
	public boolean isExtensionAllowedAndReadable(File file) {
		if (null == file || !config.isEnabled() || file.isDirectory() || !file.canRead()) {
			return false;
		}
		if (config.getAllowedExtensions().contains(getExtension(file))) {
			return true;
		}
		return false;
	}
	
	@Override
	public boolean isExtensionAllowedAndWriteable(File file) {
		if (!config.isReadOnly() && isExtensionAllowedAndReadable(file) && file.canWrite()) {
			return config.getAllowedExtensionsToEdit().contains(getExtension(file));
		}
		return false;
	}
	
	@Override
	public String readFileToString(File file, String encoding) throws IOException {
		return FileUtils.readFileToString(file, (StringUtils.isEmpty(encoding) ? config.getDefaultEncoding() : encoding));
	}
	
	@Override
	public boolean isChangeable(File file) {
		if (null != permissionHandler) {
			return permissionHandler.isChangeable(file);
		}
		if (isExtensionAllowedAndWriteable(file)) {
			return true;
		}
		return false;
	}
	
	@Override
	public void writeStringToFile(File file, String encoding, String fileContent) throws GenericFilebrowserException {
		isFileAllowed(file, true);
		try {
			FileUtils.writeStringToFile(file, fileContent, encoding, false);
		} catch (Exception e) {
			throw new GenericFilebrowserException("could not write content to file: " + e.getMessage(), e);
		}
	}
}
