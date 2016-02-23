# Extendable Admin UI for Spring Boot Web Application
> The purpose of this project is to provide an easy to integrate and extendable admin UI for Spring Boot web applications with a predefined stack of tools/dashboards which can be used out-of-the-box.

## Existing things
* `core`: providing the core functionality
* `melody`: simple iFrame integration for JavaMelody (JavaMelody servlet registration is required in own project)
* `jminix`: simple iFrame integration for Jminix MBean Browser (Jminix servlet registration is required in own project)
* `log4j2`: dashboard for all log4j2 loggers with the possibility to the log level at runtime
* `quartz`: 
..* dashboard for configuration with possibility to deactivate the scheduler
..* dashboard for configured jobs with possibility to pause/resume or fire them (not implemented yet: change jobs)
* `demo`: simple spring boot web application for showcase

## Requirements
* Java 8
* Maven 3.x.x

## Based on
* [Spring Boot ](http://projects.spring.io/spring-boot/)
* [Admin LTE](https://almsaeedstudio.com/preview)
* [Thymeleaf](http://www.thymeleaf.org/)

## Explore
1. Download the project
2. execute maven or import to IDE
3. run the Demo
4. go to localhost:8090/ or localhost:8090/admintool