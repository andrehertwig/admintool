# The FileBrowser Plugin
> The file browser could be used to inspect the system files. It could be useful for checking configuration or downloading logfiles

## Features
* `file browsing`: core functionality to browse files
* `sorting`: sort result part of table by name, size, date and type
* `downloading`: download files
* `zipping`: download one ore more files/folders as compressed zip
* `uploading`: upload files via drag'n'drop
* `deleting`: remove files and directories
* `creating`: create folders
* `info`: show file and hdd info
* `file viewer`: 
  * additional functionality to show files
  * syntax highlighting for different file types
  * change and save editable files   


![Preview image](doc/screen_filebrowser_org.png?raw=true "AdminTool FileBrowser UI")
![Preview image](doc/screen_fileviewer_org.png?raw=true "AdminTool FileViewer UI")
  
## Introduced with
* admin-tools-core:1.0.1

## Requirements, Dependencies
* commons-io
* codemirror

## Usage

**No artifact deployed right now** 

```xml

	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-core</artifactId>
		<version>1.1.5</version>
	</dependency>
	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-filebrowser</artifactId>
		<version>1.1.5</version>
	</dependency>
	
```

## Configurations

### Filebrowser
	
```ini

	# to deactivate the admin component at all
	admintool.filebrowser.enabled=true
	
	# to hide the menu item in GUI, but still callable
	admintool.filebrowser.hideMenuItem=false
	
	#Directory where to start
	admintool.filebrowser.startDir=x:\\logs
	
	# semicolon separated list of
	admintool.filebrowser.forbiddenDrives=a:\\;b:\\;c:\\
	
	# no write operations are allowed
	admintool.filebrowser.readOnly=true
	
	# to enable restrictedPaths
	admintool.filebrowser.restrictedBrowsing=false
	
	# if true only restrictedPaths are allowed, otherwise restrictedPaths are working as blacklist
	admintool.filebrowser.restrictedBrowsingIsWhitelist=true
	
	# semicolon separated list of full qualified paths either used as black or white list
	admintool.filebrowser.restrictedPaths=X:\\Programs\\cygwin\\;X:\\Programs\\cygwin64\\
	
	# for file size calculation, bytes are divided by '1024 * sizeDivisorMultiplicator ^ x'
	admintool.filebrowser.sizeDivisorMultiplicator=1000
	
	# for file size calculation, number of scale
	admintool.filebrowser.fileSizeDisplayScale=2
	
	# ZIP compression level (1-10)
	admintool.filebrowser.zipCompessionLevel=1
	
	# if zip file should be created via temp-file or pushed to ServletOutputStream directly
	# advantage of using temp file is download content-size calculation
	admintool.filebrowser.zipUseTempFile=true
	
	# temp file folder with writing rights (only used if zipUseTempFile=true)
	# if empty standard system temp folder will be used
	# if prefix 'sys:' -> resolving as system variable
	# if prefix 'env:' -> resolving as (Spring) environment variable
	# if no prefix present application assumes that's a full qualified path to temp directory
	admintool.filebrowser.zipTempDir=sys:java.io.tmpdir
	
	# to deactivate download functionality
	admintool.filebrowser.downloadAllowed=true
	
	#since 1.0.6
	# to deactivate download-compression functionality
	admintool.filebrowser.downloadCompressedAllowed=true
	
	#since 1.0.1
	# for own implementation or requires admin-tools-core-security
	#semi-colon separated list of Spring Security roles like ROLE_ANONYMOUS;ROLE_ADMIN
	admintool.filebrowser.securityRoles=
	
	#since 1.0.1
	# integer value. used by default comparator to order components
	admintool.filebrowser.componentPosition=
	
	#since 1.1.6
	# boolean: to enable action for folder creation
	admintool.filebrowser.createFolderAllowed=false
	
	#since 1.1.6
	# boolean: to enable action to delete folders
	admintool.filebrowser.delteFolderAllowed=false
	
	#since 1.1.6
	# boolean: to enable action to delete files
	admintool.filebrowser.delteFileAllowed=false
	
	#since 1.1.6
	# boolean: to enable action to upload files
	admintool.filebrowser.uploadAllowed=false
	
	#since 1.1.6
	# boolean: to enable hash generation for CRC-32 (in file-info-dialog)
	admintool.filebrowser.info.crc32=true
	
	#since 1.1.6
	# boolean: to enable hash generation for MD-5 (in file-info-dialog)
	admintool.filebrowser.info.md5=true
	
	#since 1.1.6
	# boolean: to enable hash generation for SHA-1 (in file-info-dialog)
	admintool.filebrowser.info.sha1=true
	
	#since 1.1.6
	# boolean: to enable hash generation for SHA-256 (in file-info-dialog)
	admintool.filebrowser.info.sha256=false
	
	#since 1.1.6
	# long: maximum file size the hashes are generated for (in file-info-dialog)
	admintool.filebrowser.info.maxFilesizeForHashes=1000000000
	
	#since 1.1.6
	# boolean: to enable folder size calculation (in file-info-dialog)
	admintool.filebrowser.info.countFolderSize=true
		
```

### Fileviewer

	
```ini

	# to deactivate the file view functionality
	admintool.fileviewer.enabled=true
	
	# semicolon separated list of selectable encodings to load files
	admintool.fileviewer.encodings=UTF-8;ISO-8859-1
	
	# default encoding to read files 
	admintool.fileviewer.defaultEncoding=UTF-8
	
	# if file manipulation should be allowed
	admintool.fileviewer.readOnly=true
	
	# semicolon separated list of allowed file extensions to show in file viewer
	admintool.fileviewer.allowedExtensions=txt;sql;properties;xml;xsd;wsdl;htm;html;css;js;log;md;sh;bat;cmd
	
	# codeMirror version (if CDN is used this could maybe changed)
	admintool.fileviewer.codeMirrorVersion=5.22.2
	
	# if CDN link https://cdn.jsdelivr.net/webjars/org.webjars.bower/ or https://cdn.jsdelivr.net/webjars/ should be used
	# since 1.1.4
	admintool.fileviewer.codeMirror.cdn.useBower=true
	
	# semicolon separated list of additional codeMirror libs to load 
	admintool.fileviewer.codeMirrorAddLibs=addon/edit/matchbrackets.js
		
```