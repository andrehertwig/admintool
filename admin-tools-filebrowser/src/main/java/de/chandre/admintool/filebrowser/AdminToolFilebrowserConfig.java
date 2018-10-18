package de.chandre.admintool.filebrowser;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import de.chandre.admintool.core.AdminToolConfig;

/**
 * configuration class for file uploader 
 * 
 * @author André
 * @since 1.0.0
 */
@Component("adminToolFilebrowserConfig")
public class AdminToolFilebrowserConfig implements AdminToolConfig {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolFilebrowserConfig.class);
	
	@Value("${admintool.filebrowser.enabled:true}")
	private boolean enabled;
	
	@Value("${admintool.filebrowser.hideMenuItem:false}")
	private boolean hideMenuItem;
	
	@Value("${admintool.filebrowser.startDir:}")
	private String startDir;
	
	@Value("#{'${admintool.filebrowser.forbiddenDrives:}'.split(';')}")
	private List<String> forbiddenDrives= new ArrayList<>();
	
	@Value("${admintool.filebrowser.readOnly:false}")
	private boolean readOnly;
	
	@Value("${admintool.filebrowser.restrictedBrowsing:false}")
	private boolean restrictedBrowsing;
	
	@Value("${admintool.filebrowser.restrictedBrowsingIsWhitelist:true}")
	private boolean restrictedBrowsingIsWhitelist;

	@Value("#{'${admintool.filebrowser.restrictedPaths:}'.split(';')}")
	private List<String> restrictedPaths = new ArrayList<>();
	
	@Value("${admintool.filebrowser.sizeDivisorMultiplicator:1000}")
	private long sizeDivisorMultiplicator;
	
	@Value("${admintool.filebrowser.fileSizeDisplayScale:2}")
	private byte fileSizeDisplayScale;
	
	@Value("${admintool.filebrowser.zipUseTempFile:true}")
	private boolean zipUseTempFile;
	
	@Value("${admintool.filebrowser.zipCompessionLevel:1}")
	private byte zipCompessionLevel;
	
	@Value("${admintool.filebrowser.zipTempDir:'sys:java.io.tmpdir'}")
	private String zipTempDir;
	
	@Value("${admintool.filebrowser.downloadAllowed:true}")
	private boolean downloadAllowed;
	
	@Value("${admintool.filebrowser.downloadCompressedAllowed:true}")
	private boolean downloadCompressedAllowed;
	
	@Value("#{'${admintool.filebrowser.securityRoles:}'.split(';')}")
	private Set<String> securityRoles = new HashSet<>();
	
	@Value("${admintool.filebrowser.componentPosition:}")
	private Integer componentPosition;
	
	@Value("${admintool.filebrowser.uploadAllowed:false}")
	private boolean uploadAllowed;
	
	@Value("${admintool.filebrowser.createFolderAllowed:false}")
	private boolean createFolderAllowed;
	
	@Value("${admintool.filebrowser.deleteFolderAllowed:false}")
	private boolean deleteFolderAllowed;
	
	@Value("${admintool.filebrowser.deleteFileAllowed:false}")
	private boolean deleteFileAllowed;
	
	@Value("${admintool.filebrowser.notDeletableIfNotWriteable:true}")
	private boolean notDeletableIfNotWriteable;
	
	@Value("${admintool.filebrowser.info.crc32:true}")
	private boolean infoCrc32;
	
	@Value("${admintool.filebrowser.info.md5:true}")
	private boolean infoMD5;
	
	@Value("${admintool.filebrowser.info.sha1:true}")
	private boolean infoSha1;
	
	@Value("${admintool.filebrowser.info.sha256:false}")
	private boolean infoSha256;
	
	@Value("${admintool.filebrowser.info.maxFilesizeForHashes:1000000000}")
	private long maxFilesizeForHashes;
	
	@Value("${admintool.filebrowser.info.countFolderSize:true}")
	private boolean countFolderSize;
	
	@Override
	public boolean isEnabled() {
		return this.enabled;
	}
	
	/**
	 * @return the hideMenuItem
	 */
	public boolean isHideMenuItem() {
		return hideMenuItem;
	}

	/**
	 * @return the startDir
	 */
	public File getStartDir() {
		if (StringUtils.isEmpty(this.startDir)) {
			return new File(this.getClass().getClassLoader().getResource("").getPath());
		}
		File start = new File(this.startDir);
		return start.isFile() ? start.getParentFile() : start;
	}

	/**
	 * @return the forbiddenDrives
	 */
	public List<String> getForbiddenDrives() {
		return forbiddenDrives;
	}

	/**
	 * @return the readOnly
	 */
	public boolean isReadOnly() {
		return isEnabled() && readOnly;
	}

	/**
	 * @return the restrictBrowsing
	 */
	public boolean isRestrictedBrowsing() {
		return restrictedBrowsing;
	}

	/**
	 * @return the restrictedBrowsingIsWhitelist
	 */
	public boolean isRestrictedBrowsingIsWhitelist() {
		return restrictedBrowsingIsWhitelist;
	}

	/**
	 * @return the restrictedPaths
	 */
	public List<String> getRestrictedPaths() {
		return restrictedPaths;
	}

	/**
	 * @return the sizeDivisorMultiplicator
	 */
	public long getSizeDivisorMultiplicator() {
		return sizeDivisorMultiplicator;
	}
	
	/**
	 * @return the sizeDivisorMultiplicator
	 */
	public void setSizeDivisorMultiplicator(long sizeDivisorMultiplicator) {
		this.sizeDivisorMultiplicator = sizeDivisorMultiplicator;
	}

	/**
	 * @return the fileSizeDisplayScale
	 */
	public byte getFileSizeDisplayScale() {
		return fileSizeDisplayScale;
	}
	
	public void setFileSizeDisplayScale(byte fileSizeDisplayScale) {
		this.fileSizeDisplayScale = fileSizeDisplayScale;
	}

	/**
	 * @return the zipUseTempFile
	 */
	public boolean isZipUseTempFile() {
		return zipUseTempFile;
	}

	/**
	 * @return the zipTempDir
	 */
	public String getZipTempDir() {
		if (this.zipTempDir.charAt(0) == '\''){
			return zipTempDir.substring(1).substring(0, this.zipTempDir.length() -2);
		}
		return zipTempDir;
	}

	/**
	 * @return the zipCompessionLevel
	 */
	public byte getZipCompessionLevel() {
		return zipCompessionLevel;
	}
	
	/**
	 * @return the downloadAllowed
	 */
	public boolean isDownloadAllowed() {
		return isEnabled() && downloadAllowed;
	}

	/**
	 * 
	 * @return
	 * @since 1.0.6
	 */
	public boolean isDownloadCompressedAllowed() {
		return isEnabled() && downloadCompressedAllowed;
	}

	/**
	 * @return the securityRoles
	 * @since 1.0.1
	 */
	public Set<String> getSecurityRoles() {
		return securityRoles;
	}
	
	/**
	 * @return the componentPosition
	 * @since 1.0.1
	 */
	public Integer getComponentPosition() {
		return componentPosition;
	}

	/**
	 * @param componentPosition the componentPosition to set
	 * @since 1.0.1
	 */
	public void setComponentPosition(Integer componentPosition) {
		this.componentPosition = componentPosition;
	}
	
	/**
	 * 
	 * @return
	 * @since 1.1.6
	 */
	public boolean isManipulationAllowed() {
		return !readOnly && (uploadAllowed || createFolderAllowed);
	}

	/**
	 * 
	 * @return
	 * @since 1.1.6
	 */
	public boolean isUploadAllowed() {
		return isEnabled() && uploadAllowed;
	}

	/**
	 * 
	 * @return
	 * @since 1.1.6
	 */
	public boolean isCreateFolderAllowed() {
		return isEnabled() && createFolderAllowed;
	}

	/**
	 * 
	 * @return
	 * @since 1.1.6
	 */
	public boolean isDeleteFolderAllowed() {
		return isEnabled() && deleteFolderAllowed;
	}

	/**
	 * 
	 * @return
	 * @since 1.1.6
	 */
	public boolean isDeleteFileAllowed() {
		return isEnabled() && deleteFileAllowed;
	}
	
	/**
	 * 
	 * @return
	 * @since 1.1.6.2
	 */
	public boolean isNotDeletableIfNotWriteable() {
		return notDeletableIfNotWriteable;
	}

	/**
	 * 
	 * @return
	 * @since 1.1.6
	 */
	public boolean isInfoCrc32() {
		return infoCrc32;
	}

	public void setInfoCrc32(boolean infoCrc32) {
		this.infoCrc32 = infoCrc32;
	}

	/**
	 * 
	 * @return
	 * @since 1.1.6
	 */
	public boolean isInfoMD5() {
		return infoMD5;
	}

	public void setInfoMD5(boolean infoMD5) {
		this.infoMD5 = infoMD5;
	}

	/**
	 * 
	 * @return
	 * @since 1.1.6
	 */
	public boolean isInfoSha1() {
		return infoSha1;
	}

	public void setInfoSha1(boolean infoSha1) {
		this.infoSha1 = infoSha1;
	}

	/**
	 * 
	 * @return
	 * @since 1.1.6
	 */
	public boolean isInfoSha256() {
		return infoSha256;
	}

	public void setInfoSha256(boolean infoSha256) {
		this.infoSha256 = infoSha256;
	}
	
	/**
	 * 
	 * @return
	 * @since 1.0.6
	 */
	public long getMaxFilesizeForHashes() {
		return maxFilesizeForHashes;
	}

	public void setMaxFilesizeForHashes(long maxFilesizeForHashes) {
		this.maxFilesizeForHashes = maxFilesizeForHashes;
	}
	
	/**
	 * 
	 * @return
	 * @since 1.1.6
	 */
	public boolean isCountFolderSize() {
		return countFolderSize;
	}

	public void setCountFolderSize(boolean countFolderSize) {
		this.countFolderSize = countFolderSize;
	}

	@Override
	@PostConstruct
	public void printConfig() {
		LOGGER.debug(toString());
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AdminToolFilebrowserConfig [enabled=").append(enabled).append(", hideMenuItem=")
				.append(hideMenuItem).append(", startDir=").append(startDir).append(", forbiddenDrives=")
				.append(forbiddenDrives).append(", readOnly=").append(readOnly).append(", restrictedBrowsing=")
				.append(restrictedBrowsing).append(", restrictedBrowsingIsWhitelist=")
				.append(restrictedBrowsingIsWhitelist).append(", restrictedPaths=").append(restrictedPaths)
				.append(", sizeDivisorMultiplicator=").append(sizeDivisorMultiplicator)
				.append(", fileSizeDisplayScale=").append(fileSizeDisplayScale).append(", zipUseTempFile=")
				.append(zipUseTempFile).append(", zipCompessionLevel=").append(zipCompessionLevel)
				.append(", zipTempDir=").append(zipTempDir).append(", downloadAllowed=").append(downloadAllowed)
				.append(", downloadCompressedAllowed=").append(downloadCompressedAllowed).append(", securityRoles=")
				.append(securityRoles).append(", componentPosition=").append(componentPosition)
				.append(", uploadAllowed=").append(uploadAllowed).append(", createFolderAllowed=")
				.append(createFolderAllowed).append(", delteFolderAllowed=").append(deleteFolderAllowed)
				.append(", delteFileAllowed=").append(deleteFileAllowed).append(", infoCrc32=").append(infoCrc32)
				.append(", infoMD5=").append(infoMD5).append(", infoSha1=").append(infoSha1).append(", infoSha256=")
				.append(infoSha256).append(", maxFilesizeForHashes=").append(maxFilesizeForHashes)
				.append(", countFolderSize=").append(countFolderSize).append("]");
		return builder.toString();
	}
}
