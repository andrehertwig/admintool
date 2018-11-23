package de.chandre.admintool.filebrowser;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

import org.springframework.beans.factory.annotation.Autowired;

/**
 * 
 * @author Andre
 *
 */
public abstract class AbstractFileBrowserService {
	
	@Autowired
	private AdminToolFilebrowserConfig config;
	
	public String encodeURL(String path) throws UnsupportedEncodingException {
		return URLEncoder.encode(path, "UTF-8");
	}
	
	/**
	 * checks file against configured path restrictions and read-write configuration<br>
	 * doesn't check file permissions
	 * 
	 * @param path the file to show/manipulate
	 * @param write if current action want to change the file
	 * @param configReadOnly if configuration value read-only is true
	 * @return true if accessible
	 * @throws IOException
	 */
	public boolean isAllowed(File path, boolean write, boolean configReadOnly) throws IOException {
		try {
			if (configReadOnly && write) return false;
			if (config.isRestrictedBrowsing()) {
				if (null != config.getRestrictedPaths() && null != path) {
					for (String restricedPath : config.getRestrictedPaths()) {
						if(path.getCanonicalPath().startsWith(restricedPath)) {
							return config.isRestrictedBrowsingIsWhitelist();
						}
					}
				}
				return !config.isRestrictedBrowsingIsWhitelist();
			}
		} catch (IOException e) {
			throw new IOException("Could not check if path '" + path.getAbsolutePath() + "' is allowed ", e);
		}
		return true;
	}
	
	protected abstract boolean isAllowedInternal(File path, boolean write, boolean configReadOnly) throws IOException, GenericFilebrowserException;
	
	
	/**
	 * returns the file extension by filename separated by last dot
	 * @param file
	 * @return null or extension
	 */
	public String getExtension(File file) {
		return getExtension(file.getName());
	}
	
	/**
	 * returns the file extension by filename separated by last dot
	 * @param fileName
	 * @return null or extension
	 */
	public String getExtension(String fileName) {
		if (fileName.lastIndexOf('.') > -1) {
			return (fileName.substring(fileName.lastIndexOf('.') + 1, fileName.length())).toLowerCase();
		}
		return null;
	}

}
