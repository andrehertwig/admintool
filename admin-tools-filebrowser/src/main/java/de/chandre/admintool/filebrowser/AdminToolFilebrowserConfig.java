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
	
	@Value("${admintool.filebrowser.createFolderAllowed:true}")
	private boolean createFolderAllowed;
	
	@Value("${admintool.filebrowser.delteFolderAllowed:true}")
	private boolean delteFolderAllowed;
	
	@Value("${admintool.filebrowser.delteFileAllowed:true}")
	private boolean delteFileAllowed;
	
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
		return readOnly;
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
	 * @return the fileSizeDisplayScale
	 */
	public byte getFileSizeDisplayScale() {
		return fileSizeDisplayScale;
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
		return downloadAllowed;
	}

	/**
	 * 
	 * @return
	 * @since 1.0.6
	 */
	public boolean isDownloadCompressedAllowed() {
		return downloadCompressedAllowed;
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
	 * @since 1.0.6
	 */
	public boolean isManipulationAllowed() {
		return !readOnly && (uploadAllowed || createFolderAllowed);
	}

	/**
	 * 
	 * @return
	 * @since 1.0.6
	 */
	public boolean isUploadAllowed() {
		return uploadAllowed;
	}

	/**
	 * 
	 * @return
	 * @since 1.0.6
	 */
	public boolean isCreateFolderAllowed() {
		return createFolderAllowed;
	}

	/**
	 * 
	 * @return
	 * @since 1.0.6
	 */
	public boolean isDelteFolderAllowed() {
		return delteFolderAllowed;
	}

	/**
	 * 
	 * @return
	 * @since 1.0.6
	 */
	public boolean isDelteFileAllowed() {
		return delteFileAllowed;
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
				.append(createFolderAllowed).append(", delteFolderAllowed=").append(delteFolderAllowed)
				.append(", delteFileAllowed=").append(delteFileAllowed).append("]");
		return builder.toString();
	}
}
