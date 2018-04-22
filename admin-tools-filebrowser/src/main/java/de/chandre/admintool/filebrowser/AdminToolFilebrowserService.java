package de.chandre.admintool.filebrowser;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.springframework.web.multipart.MultipartFile;

/**
 * interface for file browser service
 * @author Andre
 *
 */
public interface AdminToolFilebrowserService {
	
	/**
	 * checks if file is allowed to access
	 * 
	 * @param path
	 * @param write
	 * @param configReadOnly
	 * @return
	 * @throws IOException
	 */
	public boolean isAllowed(File path, boolean write, boolean configReadOnly) throws IOException;
	
	/**
	 * to url-encode a string
	 *  
	 * @param path
	 * @return
	 * @throws UnsupportedEncodingException
	 */
	String encodeURL(String path) throws UnsupportedEncodingException;

	/**
	 * returns all root directories
	 * @return
	 */
	Collection<String> getRootDirs();
	
	/**
	 * returns if the currentDir starts with rootDir  
	 * @param rootDir
	 * @param currentDir
	 * @return
	 */
	boolean isRootActive(String rootDir, String currentDir);

	/**
	 * returns the parent of directory
	 * @param dir
	 * @return
	 * @throws IOException
	 */
	String getParent(String dir) throws IOException;

	/**
	 * lists all directories within the directory
	 * @param currentDir
	 * @param sortCol
	 * @param sortAsc
	 * @return
	 * @throws IOException
	 */
	List<File> getDirectories(String currentDir, SortColumn sortCol, Boolean sortAsc, String filter) throws IOException;

	/**
	 * lists all files within the directory
	 * @param currentDir
	 * @param sortCol
	 * @param sortAsc
	 * @return
	 * @throws IOException
	 */
	List<File> getFiles(String currentDir, SortColumn sortCol, Boolean sortAsc, String filter) throws IOException;
	
	/**
	 * @param currentDir
	 * @return
	 */
	String getDirOrRootName(File currentDir);
	
	/**
	 * return a ordered list of files of directories  
	 * @param currentDir
	 * @return
	 */
	List<File> getBreadcrumb(String currentDir);

	/**
	 * @see #downloadFile(String, HttpServletResponse, String)
	 * @param filePath
	 * @param response
	 * @param if content-disposition: attachment should be set to header
	 * @throws DownloadNotAllowedException
	 * @throws GenericFilebrowserException
	 */
	void downloadFile(String filePath, HttpServletResponse response, boolean asAttachment)
			throws DownloadNotAllowedException, GenericFilebrowserException;

	/**
	 * put's the file to servlet output stream
	 * @param filePath
	 * @param response
	 * @param alternativeFileName 
	 * @param if content-disposition: attachment should be set to header
	 * @throws DownloadNotAllowedException
	 * @throws GenericFilebrowserException
	 */

	void downloadFile(String filePath, HttpServletResponse response, String alternativeFileName, boolean asAttachment)
			throws DownloadNotAllowedException, GenericFilebrowserException;
	
	/**
	 * creates a zip of given files and put's it to servlet output stream
	 * 
	 * @param filePaths
	 * @param response
	 * @throws GenericFilebrowserException
	 */
	void downloadFilesAsZip(List<String> filePaths, HttpServletResponse response) throws GenericFilebrowserException;

	/**
	 * returns the sum of file size of all files within the directory
	 * @param dir
	 * @return
	 * @throws IOException
	 */
	String getFileSizeSum(String dir, String filter) throws IOException;

	/**
	 * returns the last change date
	 * @param file
	 * @return
	 * @throws IOException
	 */
	Date getLastChange(File file) throws IOException;

	/**
	 * return the fileType ("DIR" or file extension)
	 * @param file
	 * @return
	 */
	String getFileType(File file);

	/**
	 * returns the file size
	 * @param file
	 * @return
	 */
	String getFileSize(File file);

	/**
	 * return "up",  "down" or empty string
	 * @param current
	 * @param sortCol
	 * @param sortAsc
	 * @return
	 */
	String getSortDirection(int current, SortColumn sortCol, Boolean sortAsc);

	/**
	 * 
	 * @param file
	 * @return
	 */
	String accessibleCSS(File file);

	/**
	 * 
	 * @param path
	 * @throws IOException
	 * @throws GenericFilebrowserException
	 */
	String createFolder(String path, String folderName) throws IOException, GenericFilebrowserException;

	/**
	 * deletes a file or folder
	 * 
	 * @param path
	 * @return
	 * @throws IOException
	 * @throws GenericFilebrowserException
	 */
	String deleteResource(String path) throws IOException, GenericFilebrowserException;

	/**
	 * returns a map with gathered file information 
	 * @param path
	 * @return
	 * @throws IOException
	 */
	Map<String, Object> getFileInfo(String path) throws IOException;

	/**
	 * calculates the file size with configured parameters, except that it's scaled additionally
	 * 
	 * @param fileLength
	 * @return
	 */
	String getFileSize(long fileLength);

	/**
	 * calculates the file size like {@link FileUtils}, except that it's scaled additionally
	 * 
	 * @param fileLength
	 * @return
	 */
	String getNormalFileSize(long fileLength);

	/**
	 * 
	 * @param decodedPath
	 * @param upload
	 * @return
	 * @throws IOException
	 * @throws GenericFilebrowserException
	 * @since 1.1.6
	 */
	boolean saveFile(String decodedPath, MultipartFile upload) throws IOException, GenericFilebrowserException;
}
