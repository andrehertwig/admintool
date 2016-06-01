# The FileBrowser Plugin
> The file browser could be used to inspect the system files. It could be useful for checking configuration or downloading logfiles

## Features
* `file browsing`: core functionality to browse files
* `sorting`: sort result part of table by name, size, date and type
* `downloading`: download files
* `zipping`: download one ore more files/folders as compressed zip


## Requirements, Dependencies
* admin-tools-core:1.0.1
* commons-io

## Usage

## Configurations

	
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
	
