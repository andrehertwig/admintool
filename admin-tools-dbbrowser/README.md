# The Database Browser Plugin
> The database browser plugin to query databases
> will load the DataSources associated Spring Context.


## Features
* `query`: 
  * query databases
  * working with more than one data source
  * show CLOBs with expected encoding
  * show BLOBs
  * restrict result set length
* `assistance`: syntax highlighting and code completion (since 1.0.2 also for table names)
* `tabbed browsing`: it's possible to create more SQL query tabs
* `examples`: create data source specific sql examples which could be selected within the frontend 
* `encode query`: if you don't want to transfer the query in human readable form, it's possible to base64 encode the query (since 1.0.2)

Result will be displayed via jquery.datatables 

![Preview image](doc/screen_dbbrowser_org2.png?raw=true "AdminTool Database-Browser UI")

## Introduced with
* admin-tools-core:1.0.0

## Requirements, Dependencies
* commons-io
* codemirror

## Usage

```xml
<dependency>
	<groupId>de.chandre.admin-tools</groupId>
	<artifactId>admin-tools-core</artifactId>
	<version>1.1.6.1</version>
</dependency>
<dependency>
	<groupId>de.chandre.admin-tools</groupId>
	<artifactId>admin-tools-dbbrowser</artifactId>
	<version>1.1.6.1</version>
</dependency>
```

### Manually adding DataSources

```java
@Autowired
private AdminToolDBBrowserService dbbService;

private boolean addDataSource(DataSource datasource) {
	dbbService.getDatasources().put("myOtherDataSource", datasource)
}
```

### Creating SQL Examples 
Examples must be managed per dataSource

```java
@Autowired
private AdminToolDBBrowserExampleLoader exampleLoader;

@PostConstruct
private boolean createExampleStatements() {
	ExampleStatements statements = new ExampleStatements();
	statements.setDatasourceName("dataSource");
	//add a example: first param is a cluster name (used as option group in UI), secend param is the example itself with a name and the statement
	statements.addExample("Common Tables", new ExampleStatement("SELECT * from LOGGING", "Select all from Logging table"));
	statements.addExample("Maintenance", new ExampleStatement("SELECT * from SCHEMA_VERSION", "Show Flyway migrations"));
	statements.addExample("Maintenance", new ExampleStatement("SELECT * from USER_JOBS", "Show database jobs"));
	exampleLoader.addExamples(statements);
}
```

## Configurations

Following the default values are shown.	
```ini

# to deactivate the admin component at all
admintool.dbbrowser.enabled=true

# to hide the menu item in GUI, but still callable
admintool.dbbrowser.hideMenuItem=false

# if data manipulation is allowed 
admintool.dbbrowser.dmlAllowed=false

# semicolon separated list of selectable encodings for reading CLOB fields
admintool.dbbrowser.clobEncodings=UTF-8;ISO-8859-1

# codeMirror version (if CDN is used this could maybe changed)
admintool.dbbrowser.codeMirrorVersion=5.27.4

# if CDN link https://cdn.jsdelivr.net/webjars/org.webjars.bower/ or https://cdn.jsdelivr.net/webjars/ should be used
# since 1.1.4
admintool.dbbrowser.codeMirror.cdn.useBower=true

# since 1.0.1
# semicolon separated list of additional codeMirror libs to load 
admintool.dbbrowser.codeMirrorAddLibs=addon/edit/matchbrackets.js

# since 1.0.1
# for own implementation or requires admin-tools-core-security
# semi-colon separated list of Spring Security roles like ROLE_ANONYMOUS;ROLE_ADMIN
admintool.dbbrowser.securityRoles=

# since 1.0.1
# integer value. used by default comparator to order components
admintool.dbbrowser.componentPosition=

# since 1.0.3
# boolean value. In large database (e.g. if user can see all schemas in an oracle) a exception could happen while fetching metadata.
# This will always shown as error after starting the dbBrowser. To deactivate this error message ste this to true.
admintool.dbbrowser.showMetaDataLoadException=false

# since 1.1.6
admintool.dbbrowser.loadMetaData=true

```