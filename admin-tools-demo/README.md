# The Demo-Project
> A Simple Spring-Boot project to show the tools

## Introduced with
* admin-tools-core:1.0.0

## Usage

1. Download the project
2. execute a `mvn clean install` or import to your IDE
3. run the demo JAR or start within your IDE
..1. you can add system parameter for log file folder: -Dapp.logdir=&lt;absolute-path-to-logfilefolder&gt;
4. go to localhost:8090/ or localhost:8090/admintool

You can change the application configuration (like port etc.) in /src/main/resources/application.properties 

Some URLs are secured
* `/monitoring`: operator/operator or admin/admin (also within the adminTool)
* `/jmx/`: operator/operator or admin/admin (also within the adminTool)
* `/admintool/dbbrowser`: admin/admin (within the adminTool)
