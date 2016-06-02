# Extendable Admin UI for Spring Boot Web Application
> The purpose of this project is to provide an easy to integrate and extendable admin UI for Spring Boot web applications with a predefined stack of tools/dashboards which can be used out-of-the-box.

## Existing things
* [Core](admin-tools-core/): providing the core functionality
* [JavaMelody integration](admin-tools-melody/): simple iFrame integration for JavaMelody (JavaMelody servlet registration is required in own project)
* [Jminix integration](admin-tools-jminix/): simple iFrame integration for Jminix MBean Browser (Jminix servlet registration is required in own project)
* [Log4j management](admin-tools-log4j2/): dashboard for all log4j2 loggers with the possibility to the log level at runtime
* [Quartz scheduler management](admin-tools-quartz/): 
  * dashboard for configuration with possibility to deactivate the scheduler
  * dashboard for configured jobs with possibility to pause/resume or fire them (not implemented yet: change jobs)
* [Database browser](admin-tools-dbbrowser/): database browser to access the data sources associated with spring
* [File browser](admin-tools-filebrowser/): 
  * browsing and downloading (direct/zipped) files and directories 
  * showing and editing files 
* [Spring Boot Demo application](admin-tools-demo/): simple spring boot web application for showcase

## Requirements
* Java 8
* Maven 3.2.x+

## Based on
* [Spring Boot ](http://projects.spring.io/spring-boot/)
* [Admin LTE](https://almsaeedstudio.com/preview)
* [Thymeleaf](http://www.thymeleaf.org/)

## Explore
1. Download the project
2. execute a `mvn clean install` or import to IDE
3. run the Demo JAR
4. go to localhost:8090/ or localhost:8090/admintool

Some URLs are secured
* `/monitoring`: operator/operator or admin/admin (also within the adminTool)
* `/jmx/`: operator/operator or admin/admin (also within the adminTool)
* `/admintool/dbbrowser`: admin/admin (within the adminTool)