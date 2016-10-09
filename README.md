# Extendable Admin UI for Spring Boot Web Application
> The purpose of this project is to provide an easy to integrate and extendable admin UI for Spring Boot web applications with a predefined stack of tools/dashboards which can be used out-of-the-box.

## Existing components
* [Core](admin-tools-core/): providing the core functionality
* [Core-Security](admin-tools-core-security/) since 1.0.1: Overrides some templates and provides a login template
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
* [Property Visualization](admin-tools-properties) since 1.0.1: shows Git properties and Spring environment properties
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

# Usage

## Dependency and Configuration
Include the dependencies to your dependency management. You can find it in [Maven Central](https://mvnrepository.com/artifact/de.chandre.admin-tools).
```xml

	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-core</artifactId>
		<version>1.0.1</version>
	</dependency>
	...
```

To get components scanned add the package to @ComponentScan
```java

	//required in own application to get admintool scanned  
	@ComponentScan(basePackages={"de.chandre.admintool"})

```

If you'r using Spring Security you should disable CSRF support for some tools (e.g. quartz and log4j)
```java

	protected void configure(HttpSecurity http) throws Exception {
		http
			.csrf().disable();
	}

```

## Adding own pages

### Creating a AdminComponent 
The AdminComponent the main component for configuring a module. It must contain a menu entry.
```java

	AdminComponent component = new AdminComponentImpl();
	component.setDisplayName("Demo-App-Component");
	component.addNotificationTemplate("notifications/notification");
	component.addSecurityRole("ROLE_ANONYMOUS");
	component.addSecurityRole("ROLE_ADMIN");
	
	MenuEntry mainMenu = new MenuEntry();
	mainMenu.setDisplayName("Demo-App-Component");
	mainMenu.setName("demo");
	mainMenu.setTarget("content/mycomponent");
	mainMenu.addVariable("message", "Welcome to your Dashboard");
	component.setMainMenu(mainMenu);

```

Each menu entry could have sub menu entries. Because of the AdminLTE implementation a menu entry with an existing sub menu could not be displayed anymore (the target would be useless). 
```java

	MenuEntry mainMenu = new MenuEntry();
	mainMenu.setDisplayName("Demo-App-Component");
	mainMenu.setName("demo");
	mainMenu.setTarget("");
	mainMenu.addVariable("message", "Welcome to your Dashboard");
	component.setMainMenu(mainMenu);
	
	//adding sub menu entries
	mainMenu.addSubmenuEntry(new MenuEntry("dashboard", "Dashboard", "content/dashboard"));
	mainMenu.addSubmenuEntry(new MenuEntry("dashboard2", "Dashboard 2", "content/dashboard2"));
		
	//adding a new sub menu tree
	MenuEntry submenu = new MenuEntry("", "SubMulti", "");
	submenu.addSubmenuEntry(new MenuEntry("dashboard3", "Dashboard 3", "content/dashboard3"));
	submenu.addSubmenuEntry(new MenuEntry("dashboard4", "Dashboard 4", "content/dashboard4"));
	mainMenu.addSubmenuEntry(submenu);

```

### Creating a content template

A content template must at least contain a block element with *id="template-content"*, otherwise the content will not be found by Thymeleaf. Using the namespace within HTML tag provides code completion in Eclipse IDE with installed Thymeleaf plugin.   
```html

	<!DOCTYPE html>
	<html xmlns:th="http://www.thymeleaf.org">
		<body>
			<div id="template-content" >
				<section class="content-header">
					<h1>My Fabulous Component</h1>
					<ol class="breadcrumb" th:replace="admintool/fragments/fragements :: breadcrumb"></ol>
				</section>
				<section class="content">
					... my content ...
				</section>
			</div>
		</body>
	</html>

```

The reason for this special id *id="template-content"* is that the index.html within the core module will include the content page content by this particular id `<th:block th:include="${contentPage} :: #template-content" />`. So all other HTML tags outside the block element with its special id will not be considered.

#### Since 1.0.2-SNAPSHOT
The template resolution has been restructured a bit. 
Now the [Thymeleaf Layout dialect](http://www.thymeleaf.org/doc/articles/layouts.html#thymeleaf-layout-dialect) will be used.
So a template should look like this:
```html

	<!DOCTYPE html>
	<html xmlns:th="http://www.thymeleaf.org" layout:decorator="admintool/layout/standardLayout">
		<body>
			<div layout:fragment="content">
				<section class="content-header">
					<h1>My Fabulous Component</h1>
					<ol class="breadcrumb" th:replace="admintool/fragments/fragements :: breadcrumb"></ol>
				</section>
				<section class="content">
					... my content ...
				</section>
			</div>
		</body>
	</html>

```

So it will be much easier to apply a custom layout.

### Template resolution 
The Thymeleaf templates will be resolved in the *admintool* folder of configured template folder.
e.g if configured Thymeleaf root template folder is *templates* all templates all own templates should be places within: 
`/src/main/resources/templates/admintool/`
Commonly provided admin-tools modules will have the following structure
* `../admintool/<component>/content/..`
* `../admintool/<component>/includes/..`
* `../admintool/<component>/.../..`

beside the core. It will have:
* `../admintool/index.html`
* `../admintool/content/...`
* `../admintool/fragments/...`
* `../admintool/includes/...`


The *MenuEntry.target* should point relative from *admintool* folder to the template which should be resolved by this menu entry. This template will be shown in the central area (within tag `<div class="content-wrapper">`).

You can also override templates provided by admin-tools-core library. Per default templates will be found by *OrderedClassLoaderResourceResolver* using the *TemplateUrlComparator*. If more than one template has been found the core template will be used last, all others in natural string comparison of absolute template URL (with JAR names) -> The first will be picked. 
