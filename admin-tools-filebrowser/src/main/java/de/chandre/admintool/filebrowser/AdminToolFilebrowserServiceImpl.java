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
import java.math.BigInteger;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.DosFileAttributes;
import java.nio.file.attribute.FileTime;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import de.chandre.admintool.core.utils.RegexUtil;

/**
 * 
 * @author Andre
 * @since 1.0.1
 */
@Service("adminToolFilebrowserService")
public class AdminToolFilebrowserServiceImpl extends AbstractFileBrowserService implements AdminToolFilebrowserService {

	private static final Log LOGGER = LogFactory.getLog(AdminToolFilebrowserServiceImpl.class);
	
	private static final Map<Integer, String> FILE_SIZE_EXP = new TreeMap<>();
	static {
		FILE_SIZE_EXP.put(Integer.valueOf(1), " B");
		FILE_SIZE_EXP.put(Integer.valueOf(2), " KB");
		FILE_SIZE_EXP.put(Integer.valueOf(3), " MB");
		FILE_SIZE_EXP.put(Integer.valueOf(4), " GB");
		FILE_SIZE_EXP.put(Integer.valueOf(5), " TB");
		FILE_SIZE_EXP.put(Integer.valueOf(6), " PB");
		FILE_SIZE_EXP.put(Integer.valueOf(7), " EB");
	}
	
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
	public boolean isRootActive(String rootDir, String currentDir) {
		if (!StringUtils.isEmpty(currentDir) && currentDir.toLowerCase().startsWith(rootDir.toLowerCase())) {
			return true;
		}
		return false;
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
	public List<File> getDirectories(String currentDir, SortColumn sortCol, Boolean sortAsc, String filter) throws IOException {
		File file = new File(currentDir);
		if (null != file && isAllowed(file, false)) {
			final Pattern fileNamePattern = getFileNamePattern(filter);
			File[] files = file.listFiles(new FileFilter() {
				@Override
				public boolean accept(File dir) {
					try {
						if (isAllowed(dir, false) && dir.isDirectory()) {
							if (null == fileNamePattern) {
								return true;
							} else if (null != fileNamePattern && fileNamePattern.matcher(dir.getName()).matches()) {
								return true;
							}
						}
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
	public List<File> getFiles(String currentDir, SortColumn sortCol, Boolean sortAsc, String filter) throws IOException {
		File file = new File(currentDir);
		if (null != file && isAllowed(file, false)) {
			final Pattern fileNamePattern = getFileNamePattern(filter);
			File[] files = file.listFiles(new FileFilter() {
				@Override
				public boolean accept(File dir) {
					try {
						if (isAllowed(dir, false) && dir.isFile()) {
							if (null == fileNamePattern) {
								return true;
							} else if (null != fileNamePattern && fileNamePattern.matcher(dir.getName()).matches()) {
								return true;
							}
						}
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
	
	private Pattern getFileNamePattern(String filter) {
		if (!StringUtils.isEmpty(filter)) {
			return Pattern.compile(RegexUtil.wildcardToRegex(filter), Pattern.CASE_INSENSITIVE);
		}
		return null;
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
		if (null == currentDir) {
			return Collections.emptyList();
		}
		List<File> result = new ArrayList<>();
		File file = new File(currentDir);
		getParentsRecursive(file, result);
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
	public String getFileSizeSum(String dir, String filter) throws IOException {
		List<File> files = getFiles(dir, null, true, filter);
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
		return getExtension(file);
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
	@Override
	public String getFileSize(long fileLength) {
		return calculateDisplayFileSize(fileLength, config.getSizeDivisorMultiplicator());
	}
	
	@Override
	public String getNormalFileSize(long fileLength) {
		return calculateDisplayFileSize(fileLength, FileUtils.ONE_KB);
	}
	
	protected String calculateDisplayFileSize(long fileLength, long sizeDivisorMultiplicator) {
		BigInteger multiplicator = BigInteger.valueOf(sizeDivisorMultiplicator);
		BigInteger size = BigInteger.valueOf(fileLength);
		
		for (Entry<Integer, String> entry : FILE_SIZE_EXP.entrySet()) {
			BigInteger divisor = multiplicator.pow(entry.getKey().intValue());
			if (fileLength < divisor.longValue()) {
				if (entry.getKey().intValue() == 1) {
					return  String.valueOf(size) + entry.getValue();
				}
				return formatFileSize(size, multiplicator.pow(entry.getKey().intValue() - 1), entry.getValue());
			}
		}
		//should not happen
		return FileUtils.byteCountToDisplaySize(fileLength);
	}
	
	/**
	 * calculates the and formats files size
	 * @see #getFileSize(long)
	 * @param fileLength
	 * @param divisor 
	 * @param unit the Unit for the divisor
	 * @return
	 */
	protected String formatFileSize(BigInteger fileLength, BigInteger divisor, String unit) {
		BigDecimal size = new BigDecimal(fileLength);
		size = size.setScale(config.getFileSizeDisplayScale()).divide(new BigDecimal(divisor), BigDecimal.ROUND_HALF_EVEN);
		return String.format("%s %s", size.doubleValue(), unit);
	}
	
	/**
	 * 
	 * @param fileName the name
	 * @param size (optional) size/length of content
	 * @param response the servlet response
	 */
	protected void prepareDownloadResponse(String fileName, Long size, HttpServletResponse response, boolean asAttachment) {
		
		String mimeType = MimeTypes.getMimeType(getExtension(fileName));
		LOGGER.info("following mimeType:" + mimeType);
		if (asAttachment || null == mimeType) {
			response.setContentType("application/octet-stream");
			response.setHeader("Content-Disposition", "attachment;filename=\"" + fileName + "\"");
		} else {
			response.setContentType(mimeType);
			response.setHeader("Content-Disposition", "inline;filename=\"" + fileName + "\"");
		}
		if (null != size) {
			response.setContentLength(size.intValue());
		}
	}
	
	@Override
	public void downloadFile(String filePath, HttpServletResponse response, boolean asAttachment) throws DownloadNotAllowedException, GenericFilebrowserException {
		downloadFile(filePath, response, null, asAttachment);
	}
	
	@Override
	public void downloadFile(String filePath, HttpServletResponse response, String alternativeFileName, boolean asAttachment) throws DownloadNotAllowedException, GenericFilebrowserException {
		File file = new File(filePath);
		try {
			if (!isAllowed(file, false)) {
				throw new DownloadNotAllowedException();
			}
		} catch (IOException e) {
			throw new GenericFilebrowserException(e);
		}
		
		prepareDownloadResponse(StringUtils.isEmpty(alternativeFileName) ? file.getName() : alternativeFileName, 
				Long.valueOf(file.length()), response, asAttachment);
		
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
				prepareDownloadResponse("rename_me.zip", null, response, true);
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
				downloadFile(tempFile.getAbsolutePath(), response, "rename_me.zip", true);
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
	
	@Override
	public String accessibleCSS(File file) {
		String res = "";
		if (!file.canRead()) {
			res += "not-readable";
		}
		if (!file.canWrite()) {
			res += " not-writeable";
		}
		if (file.isHidden()) {
			res += " file-hidden";
		}
		return res.trim();
	}
	
	@Override
	public String createFolder(String path, String folderName) throws IOException, GenericFilebrowserException {
		File file = new File(path);
		if (file.exists()) {
			if (isAllowed(file, true)) {
				file = new File(file, folderName);
				if(file.mkdirs()) {
					return file.getAbsolutePath();
				}
				throw new GenericFilebrowserException("could not create directories");
			}
		} else {
			LOGGER.warn("[createFolder] folder already exists: " + path);
		}
		return null;
	}
	
	@Override
	public String deleteResource(String path) throws IOException, GenericFilebrowserException {
		File file = new File(path);
		
		if (file.isDirectory() && !config.isDelteFolderAllowed()) {
			throw new GenericFilebrowserException("delete folder is not allowed");
		}
		if (file.isFile() && !config.isDelteFileAllowed()) {
			throw new GenericFilebrowserException("delete file is not allowed");
		}
		if (!isAllowed(file, true)) {
			throw new GenericFilebrowserException("delete "+(file.isDirectory() ?  "folder" : "file")+" is not allowed");
		}
		
		String parent = file.getParent();
		
		if (file.isFile()) {
			FileUtils.deleteQuietly(file);
		} else {
			try {
				FileUtils.deleteDirectory(file);
			} catch (Exception e) {
				LOGGER.error("error deleting folder", e);
			}
		}
		return parent;
	}
	
	@Override
	public Map<String, Object> getFileInfo(String path) throws IOException {
		File file = new File(path);
		Map<String, Object> result = new TreeMap<>();
		result.put("file", file);
		
		if (file.isFile() && file.canRead() && config.getMaxFilesizeForHashes() > file.length()) {
			if (config.isInfoCrc32()) {
				result.put("file.checksumCRC32", FileUtils.checksumCRC32(file));
			}
			
			FileInputStream fis = new FileInputStream(file);
			if (config.isInfoMD5()) {
				result.put("file.md5Hex", DigestUtils.md5Hex(fis));
			}
			if (config.isInfoSha1()) {
				result.put("file.sha1Hex", DigestUtils.sha1Hex(fis));
			}
			if (config.isInfoSha256()) {
				result.put("file.sha256Hex", DigestUtils.sha256Hex(fis));
			}
//			result.put("file.sha384Hex", DigestUtils.sha384Hex(fis));
//			result.put("file.sha512Hex", DigestUtils.sha512Hex(fis));
			
			IOUtils.closeQuietly(fis);
		}
		
		result.put("file.lastModified", file.lastModified());
		result.put("file.canWrite", file.canWrite());
		result.put("file.canRead", file.canRead());
		result.put("file.canExecute", file.canExecute());
		result.put("file.isHidden", file.isHidden());
		
		result.put("disk.totalSpace", file.getTotalSpace());
		result.put("disk.usableSpace", file.getUsableSpace());
		result.put("disk.freeSpace", file.getFreeSpace());
		result.put("disk.totalSpace.coreFormat", getFileSize(file.getTotalSpace()));
		result.put("disk.usableSpace.coreFormat", getFileSize(file.getUsableSpace()));
		result.put("disk.freeSpace.coreFormat", getFileSize(file.getFreeSpace()));
		result.put("disk.totalSpace.commonFormat", getNormalFileSize(file.getTotalSpace()));
		result.put("disk.usableSpace.commonFormat", getNormalFileSize(file.getUsableSpace()));
		result.put("disk.freeSpace.commonFormat", getNormalFileSize(file.getFreeSpace()));
		
		String os = System.getProperty("os.name").toLowerCase();
		result.put("system.operationSystem", os);
		
		Class<? extends BasicFileAttributes> attributesClass = 
				isWindows(os) ? DosFileAttributes.class : BasicFileAttributes.class;
		BasicFileAttributes attr = Files.readAttributes(file.toPath(), attributesClass);
		
		result.put("file.attr.creationTime", attr.creationTime());
		result.put("file.attr.lastAccessTime", attr.lastAccessTime());
		result.put("file.attr.lastModifiedTime", attr.lastModifiedTime());

		result.put("file.attr.isDirectory", attr.isDirectory());
		result.put("file.attr.isOther", attr.isOther());
		result.put("file.attr.isRegularFile", attr.isRegularFile());
		result.put("file.attr.isSymbolicLink", attr.isSymbolicLink());
		result.put("file.attr.size", attr.size());
		
		
		if (DosFileAttributes.class.isAssignableFrom(attr.getClass())) {
			result.put("file.attr.isArchive", ((DosFileAttributes)attr).isArchive());
			result.put("file.attr.isHidden", ((DosFileAttributes)attr).isHidden());
			result.put("file.attr.isReadOnly", ((DosFileAttributes)attr).isReadOnly());
			result.put("file.attr.isSystem", ((DosFileAttributes)attr).isSystem());
		}
		
		long size = file.length();
		if (file.isDirectory() && !attr.isSymbolicLink() && config.isCountFolderSize()) {
			size = FileUtils.sizeOfDirectory(file);
		}
		result.put("file.size", size);
		result.put("file.size.coreFormat", getFileSize(size));
		result.put("file.size.commonFormat", getNormalFileSize(size));
		
		if (!isWindows(os)) {
			Set<PosixFilePermission> permissions = Files.getPosixFilePermissions(file.toPath(), LinkOption.NOFOLLOW_LINKS);
			result.put("file.permissions", PosixFilePermissions.toString(permissions));
		}
		
		for (Entry<String, Object> entry : result.entrySet()) {
			System.out.println(String.format("%1$20s : %2$s", entry.getKey(), entry.getValue()));
		}
		return result;
	}
	
	public static boolean isWindows(String os) {
        return (os.indexOf("win") >= 0);
    }
	
	@Override
	public boolean saveFile(String decodedPath, MultipartFile upload) throws IOException, GenericFilebrowserException {
		if (StringUtils.isEmpty(decodedPath)) {
			return false;
		}
		File uploadFolder = new File(decodedPath);
		if (uploadFolder.isDirectory() && isAllowed(uploadFolder, true)) {
			
			OutputStream fos = null;
			try {
				File file = new File(uploadFolder, upload.getOriginalFilename());
				fos = new FileOutputStream(file);
				long result = IOUtils.copyLarge(upload.getInputStream(), fos);
				LOGGER.info(String.format("uploaded %s bytes (%s)", result, getNormalFileSize(result)));
			} catch (Exception e) {
				LOGGER.error(e.getMessage(), e);
				return false;
			} finally {
				IOUtils.closeQuietly(fos);
			}
			return true;
		} 
		throw new GenericFilebrowserException("upload not allowed");
	}
}
