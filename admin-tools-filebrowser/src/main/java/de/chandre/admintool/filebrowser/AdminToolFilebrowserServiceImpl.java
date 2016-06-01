package de.chandre.admintool.filebrowser;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.IOUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

/**
 * 
 * @author Andre
 * @since 1.0.1
 */
@Service("adminToolFilebrowserService")
public class AdminToolFilebrowserServiceImpl extends AbstractFileBrowserService implements AdminToolFilebrowserService {

	private static final Log LOGGER = LogFactory.getLog(AdminToolFilebrowserServiceImpl.class);
	
	@Autowired
	private AdminToolFilebrowserConfig config;
	
	@Autowired
	private Environment env;
	
	private Set<String> rootDirsCache = Collections.newSetFromMap(new ConcurrentHashMap<>());
	
	@Override
	public Set<String> getRootDirs() {
		File[] roots = File.listRoots();
		
		if (this.rootDirsCache.isEmpty()) {
			for (File file : roots) {
				if(!config.getForbiddenDrives().contains(file.getAbsolutePath().toLowerCase())) {
					// if not forbidden add it to result
					this.rootDirsCache.add(file.getAbsolutePath());
				}
			}
		}
		return this.rootDirsCache;
	}
	
	@Override
	public String getParent(String dir) throws IOException {
		File file = new File(dir);
		if (null != file.getParent() && isAllowed(file.getParentFile(), false)) {
			return file.getParent();
		}
		return "";
	}
	
	protected List<File> sort(File[] fileAr, final SortColumn sortCol, Boolean sortAsc) {
		List<File> files = Arrays.asList(fileAr);
		if (null == sortCol) {
			return files;
		}
		if (null == sortAsc) {
			sortAsc = Boolean.TRUE;
		}
		final int direction = sortAsc.booleanValue() ? 1 : -1;
		
		Collections.sort(files, new Comparator<File>() {
			@Override
			public int compare(File o1, File o2) {
				try {
					switch (sortCol) {
						case DATE:
							return getLastChange(o1).compareTo(getLastChange(o2)) * direction;
						case SIZE: 
							return Long.valueOf(o1.length()).compareTo(Long.valueOf(o2.length())) * direction;
						case TYPE: 
							return getFileType(o1).compareTo(getFileType(o2)) * direction;
						case NAME:
						default:
							return o1.getName().compareTo(o2.getName()) * direction;
					}
				} catch (Exception ignore) {
				}
				
				return 0;
			}
		});
		return files;
	}
	
	@Override
	public String getSortDirection(int current, SortColumn sortCol, Boolean sortAsc) {
		if (current == sortCol.getIndex() && sortAsc != null) {
			return sortAsc ? "up" : "down";
		}
		return "";
	}
	
	@Override
	public List<File> getDirectories(String currentDir, SortColumn sortCol, Boolean sortAsc) throws IOException {
		File file = new File(currentDir);
		if (null != file && isAllowed(file, false)) {
			File[] files = file.listFiles(new FileFilter() {
				@Override
				public boolean accept(File dir) {
					try {
						return isAllowed(dir, false) && dir.isDirectory();
					} catch (IOException e) {
						LOGGER.debug(e.getMessage(), e);
					}
					return false;
				}
			});
			if (files != null) {
				return sort(files, sortCol, sortAsc);
			}
		}
		return Collections.emptyList();
	}
	
	@Override
	public List<File> getFiles(String currentDir, SortColumn sortCol, Boolean sortAsc) throws IOException {
		File file = new File(currentDir);
		if (null != file && isAllowed(file, false)) {
			File[] files = file.listFiles(new FileFilter() {
				@Override
				public boolean accept(File dir) {
					try {
						return isAllowed(dir, false) && dir.isFile();
					} catch (IOException e) {
						LOGGER.debug(e.getMessage(), e);
					}
					return false;
				}
			});
			if (files != null) {
				return sort(files, sortCol, sortAsc);
			}
		}
		return Collections.emptyList();
	}
	
	@Override
	public String getDirOrRootName(File currentDir) {
		if (StringUtils.isEmpty(currentDir.getName())) {
			return currentDir.getAbsolutePath();
		}
		return currentDir.getName();
	}
	
	@Override
	public List<File> getBreadcrumb(String currentDir) {
		List<File> result = new ArrayList<>();
		File file = new File(currentDir);
		getParentsRecursive(file, result);
		
		//Collections.rotate(result, 1);
		return result;
	}
	
	private void getParentsRecursive(File actual, List<File> files) {
		if (null != actual.getParentFile()) {
			getParentsRecursive(actual.getParentFile(), files);
		} 
		if (actual.isDirectory()) {
			files.add(actual);
		}
	}
	
	
	
	@Override
	public String getFileSizeSum(String dir) throws IOException {
		List<File> files = getFiles(dir, null, true);
		Long res = files.stream().collect(Collectors.summingLong(File::length));
		return String.format("%s in %s files", getFileSize(res), files.size());
	}
	
	@Override
	public Date getLastChange(File file) throws IOException {
		if (null != file) {
			FileTime time = Files.getLastModifiedTime(file.toPath(), new LinkOption[]{});
			return new Date(time.toMillis());
		}
		return new Date();
	}
	
	@Override
	public String getFileType(File file) {
		if (null == file) {
			return "";
		}
		if (file.isDirectory()) {
			return "DIR";
		}
		if (file.getName().lastIndexOf('.') == -1) {
			return "";
		}
		return file.getName().substring(file.getName().lastIndexOf('.'), file.getName().length());
	}
	
	@Override
	public String getFileSize(File file) {
		return getFileSize(file.length());
	}
	
	/**
	 * returns the the size in B, KB, MB or GB depending on the length
	 * 
	 * @param fileLength
	 * @return
	 */
	protected String getFileSize(long fileLength) {
		long multiplicator = config.getSizeDivisorMultiplicator();
		if (fileLength < 1024L) {
			return String.valueOf(fileLength) + " B";
		}
		if (fileLength < 1024L * multiplicator) {
			return getFileSize(fileLength, 1024, "KB");
		}
		if (fileLength < 1024L * multiplicator * multiplicator) {
			return getFileSize(fileLength, 1024 * multiplicator, "MB");
		}
		return getFileSize(fileLength, 1024 * multiplicator * multiplicator, "GB");
	}
	
	/**
	 * calculates the and formats files size
	 * @see #getFileSize(long)
	 * @param fileLength
	 * @param divisor 
	 * @param unit the Unit for the divisor
	 * @return
	 */
	protected String getFileSize(long fileLength, long divisor, String unit) {
		BigDecimal size = BigDecimal.valueOf(fileLength);
		size = size.setScale(config.getFileSizeDisplayScale()).divide(new BigDecimal(divisor), BigDecimal.ROUND_HALF_EVEN);
		return String.format("%s %s", size.doubleValue(), unit);
	}
	
	/**
	 * 
	 * @param fileName the name
	 * @param size (optional) size/length of content
	 * @param response the servlet response
	 */
	protected void prepareDownloadResponse(String fileName, Long size, HttpServletResponse response) {
		response.setContentType("application/octet-stream");
		response.setHeader("Content-Disposition", "attachment;filename=\"" + fileName + "\"");
		if (null != size) {
			response.setContentLength(size.intValue());
		}
	}
	
	@Override
	public void downloadFile(String filePath, HttpServletResponse response) throws DownloadNotAllowedException, GenericFilebrowserException {
		downloadFile(filePath, response, null);
	}
	
	@Override
	public void downloadFile(String filePath, HttpServletResponse response, String alternativeFileName) throws DownloadNotAllowedException, GenericFilebrowserException {
		File file = new File(filePath);
		try {
			if (!isAllowed(file, false)) {
				throw new DownloadNotAllowedException();
			}
		} catch (IOException e) {
			throw new GenericFilebrowserException(e);
		}
		
		prepareDownloadResponse(StringUtils.isEmpty(alternativeFileName) ? file.getName() : alternativeFileName, 
				Long.valueOf(file.length()), response);
		
		InputStream in = null;
		BufferedInputStream fileInput = null;
		try {
			in = new FileInputStream(file);
			fileInput = new BufferedInputStream(in);
			ServletOutputStream out = response.getOutputStream();
			IOUtils.copy(fileInput, out, 8192);
			out.flush();
		} catch (IOException e) {
			throw new GenericFilebrowserException("could not prepare file for downloading", e);
		}
		finally {
			IOUtils.closeQuietly(fileInput);
			IOUtils.closeQuietly(in);
		}
	}
	
	@Override
	public void downloadFilesAsZip(List<String> filePaths, HttpServletResponse response) throws GenericFilebrowserException {
		
		File tempFile = null;
		try {
			OutputStream out = null;
			
			try {
				if (config.isZipUseTempFile()) {
					String tempDirConf = config.getZipTempDir();
					File tempDir = null;
					if (StringUtils.isEmpty(tempDirConf)) {
						tempFile = File.createTempFile("zip", null);
					}
					else if (tempDirConf.startsWith("sys")) {
						tempDir = new File(System.getProperty(tempDirConf.substring(4, tempDirConf.length())));
					}
					else if (tempDirConf.startsWith("env")) {
						tempDir = new File(env.getProperty(tempDirConf.substring(4, tempDirConf.length())));
					}
					else {
						tempDir = new File(tempDirConf);
					}
					if (null == tempFile) {
						tempFile = File.createTempFile("zip", null, tempDir);
					}
					out = new FileOutputStream(tempFile);
				}
			} catch (Exception e) {
				LOGGER.warn("could not create temporary file, using servlet outputstream for serving ZIP");
			}
			
			if (null == out) {
				tempFile = null;
				prepareDownloadResponse("rename_me.zip", null, response);
				out = response.getOutputStream();
			}
			
			ZipOutputStream zos = new ZipOutputStream(out);
			
			zos.setLevel(config.getZipCompessionLevel());
			try {
				for (String filePathStr : filePaths) {
					File orgfile = new File(filePathStr);
					Files.walkFileTree(orgfile.toPath(), new SimpleFileVisitor<Path>() {
						public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
							if (!isAllowed(file.toFile(), false)) {
								LOGGER.debug("ZIP creation: skipping not allowed file: " + file.toAbsolutePath());
								return FileVisitResult.CONTINUE;
							}
							String entry = orgfile.getParentFile().toPath().relativize(file).toString();
							LOGGER.trace("creating entry: " + entry);
							zos.putNextEntry(new ZipEntry(entry));
							Files.copy(file, zos);
							zos.closeEntry();
							return FileVisitResult.CONTINUE;
						}

						public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
							if (!isAllowed(dir.toFile(), false)) {
								LOGGER.debug("ZIP creation: skipping not allowed directory and subtree for: " + dir.toAbsolutePath());
								return FileVisitResult.SKIP_SUBTREE;
							}
							String entry = orgfile.getParentFile().toPath().relativize(dir).toString() + "/";
							LOGGER.trace("creating dir: " + entry);
							zos.putNextEntry(new ZipEntry(entry));
							zos.closeEntry();
							return FileVisitResult.CONTINUE;
						}
					});
				}
				
				zos.finish();
				
			} catch (IOException e) {
				IOUtils.closeQuietly(zos);
				throw new GenericFilebrowserException("Could not create zip file is allowed ", e);
			}
			// flushing the output
			out.flush();
			
			if (null != tempFile && config.isZipUseTempFile()) {
				IOUtils.closeQuietly(out);
				downloadFile(tempFile.getAbsolutePath(), response, "rename_me.zip");
			}
			
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw new GenericFilebrowserException(e);
		} finally {
			if (null != tempFile && config.isZipUseTempFile()) {
				try {
					if(!tempFile.delete()) {
						tempFile.deleteOnExit();
					}
				} catch (Exception e2) {
					LOGGER.warn("could not delete tempfile " + tempFile.getAbsolutePath());
					tempFile.deleteOnExit();
				}
			}
		}
	}
	
	/**
	 * checks if file is allowed for access
	 * 
	 * @param path
	 * @param write
	 * @return
	 * @throws IOException
	 */
	protected boolean isAllowed(File path, boolean write) throws IOException {
		return isAllowed(path, write, config.isReadOnly());
	}
}
