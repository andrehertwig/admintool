package de.chandre.admintool.filebrowser;

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletResponse;

/**
 * interface for file browser service
 * @author Andre
 *
 */
public interface AdminToolFilebrowserService {

	/**
	 * returns all root directories
	 * @return
	 */
	Set<String> getRootDirs();
	
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
	List<File> getDirectories(String currentDir, SortColumn sortCol, Boolean sortAsc) throws IOException;

	/**
	 * lists all files within the directory
	 * @param currentDir
	 * @param sortCol
	 * @param sortAsc
	 * @return
	 * @throws IOException
	 */
	List<File> getFiles(String currentDir, SortColumn sortCol, Boolean sortAsc) throws IOException;
	
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
	String getFileSizeSum(String dir) throws IOException;

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
}
