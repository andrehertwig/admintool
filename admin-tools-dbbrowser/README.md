# The Database Browser Plugin
> The database browser plugin to query databases
> will load the DataSources associated Spring Context.


## Features
* `query`: query databases
* `assistance`: syntax highlighting and code completion
* `tabbed browsing` it's possible to create more SQL query tabs
* `examples`: create examples which could be loaded 

Result will be displayed via jquery.datatables 

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
		<version>1.0.0</version>
	</dependency>
	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-dbbrowser</artifactId>
		<version>1.0.0</version>
	</dependency>
	
```

Manually adding DataSources

```java

	@Autowired
	private AdminToolDBBrowserService dbbService;
	
	private boolean addDataSource(DataSource datasource) {
		dbbService.getDatasources().put("myOtherDataSource", datasource)
	}

```

Manually adding DataSources

```java

	@Autowired
	private AdminToolDBBrowserExampleLoader exampleLoader;

	@PostConstruct
	private boolean createExampleStatements() {
		ExampleStatements statements = new ExampleStatements();
		statements.setDatasourceName("dataSource");
		statements.addExample("Common Tables", new ExampleStatement("SELECT * from LOGGING", "Select all from Logging table"));
		statements.addExample("Maintenance", new ExampleStatement("SELECT * from SCHEMA_VERSION", "Show Flyway migrations"));
		exampleLoader.addExamples(statements);
	}
```

## Configurations

	# to deactivate the functionality
	admintool.dbbrowser.enabled=true
	
	# if data manipulation is allowed 
	admintool.dbbrowser.dmlAllowed=false
	
	# semicolon separated list of selectable encodings for reading CLOB fields
	admintool.dbbrowser.clobEncodings=UTF-8;ISO-8859-1
	
	# codeMirror version (if CDN is used this could maybe changed)
	admintool.dbbrowser.codeMirrorVersion:5.13.2
	
	#since 1.0.1
	# semicolon separated list of additional codeMirror libs to load 
	admintool.dbbrowser.codeMirrorAddLibs:addon/edit/matchbrackets.js
