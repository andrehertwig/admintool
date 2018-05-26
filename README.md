# Extendable Admin UI for Spring Boot Web Application
> The purpose of this project is to provide an easy to integrate and extendable admin UI for Spring Boot web applications with a predefined stack of tools/dashboards which can be used out-of-the-box.

This is just a spare-time project. The usage of this tool (especially in production systems) is at your own risk.

* Prev Release: 1.1.6.1 - 18.01.2018
* Prev Release: 1.1.6.2 - 20.03.2018
* Prev Release: 1.1.6.3 - 03.04.2018
* Prev Release: 1.1.6.4 - 08.04.2018
* Last Release: 1.1.6.5 - 26.05.2018

[![Maven Central](https://img.shields.io/maven-central/v/de.chandre.admin-tools/admin-tools-core.svg)](https://mvnrepository.com/artifact/de.chandre.admin-tools)
[![GitHub issues](https://img.shields.io/github/issues/andrehertwig/admintool.svg)](https://github.com/andrehertwig/admintool/issues)
[![license](https://img.shields.io/github/license/andrehertwig/admintool.svg)](https://github.com/andrehertwig/admintool/blob/develop/LICENSE)

**Table of contents**

1. [Existing Components](#existing-components)
2. [Requirements](#requirements)
3. [Based on](#based-on)
4. [Explore](#explore)
5. [Usage](#usage)
   1. [Dependency and Configuration](#dependency-and-configuration)
   2. [Adding own Pages](#adding-own-pages)
      1. [Creating an AdminComponent](#creating-an-admincomponent)
      2. [Creating a Content Template](#creating-a-content-template)
   3. [Template Resolution](#template-resolution)
   4. [Enable Internationalization](#enable-internationalization)
   5. [Checking the Menu Integrity](#checking-the-menu-integrity)
   6. [Packing the Core-Menu-Structure](#packing-the-core-menu-structure)
   7. [Using AdminTool Core JS ](#using-admintool-core-js)
      1. [Extending the AdminTool.Core](#extending-the-admintoolcore)
	  2. [Methods of AdminTool.Core JS](#methods-of-admintoolcore-js)

## Existing Components
* [Core](admin-tools-core/): providing the core functionality
* [Core-Security](admin-tools-core-security/) 
  * since 1.0.1: Overrides some templates and provides a login template
  * since 1.1.5: simpe User-View
* [JavaMelody integration](admin-tools-melody/): simple iFrame integration for JavaMelody (JavaMelody servlet registration is required for your own project)
* [Jminix integration](admin-tools-jminix/): 
  * (before 1.1.6) simple iFrame integration for Jminix MBean Browser (Jminix servlet registration is required for your own project)
  * (since 1.1.6) rebuild JMX-Tree browser via JS 
* [Log4j management](admin-tools-log4j2/): 
  * dashboard for all log4j2 loggers with the option to change the log level at runtime
  * since 1.1.1: web based logging console to get direct log output
* [Quartz scheduler management](admin-tools-quartz/): 
  * dashboard for configuration with option to deactivate the scheduler
  * dashboard for configured jobs with option to pause/resume or fire them (experimental: change jobs (including job data))
* [Database browser](admin-tools-dbbrowser/): database browser to access the data sources associated with spring
* [File browser](admin-tools-filebrowser/): 
  * browsing and downloading (direct/zipped) files and directories
  * since 1.1.6: file info, create directories, uploading files, delete files and directories 
  * showing and editing files 
* [Property Visualization](admin-tools-properties) since 1.0.1: shows Git properties and Spring environment properties
* [Spring Boot Demo application](admin-tools-demo-jar/): simple spring boot web application for showcase
* [Spring Boot Demo Tomcat application](admin-tools-demo-war/): simple spring boot web application for showcase in tomcat

## Requirements
* Java 8
* Maven 3.2.x+

## Based on
* [Spring Boot 1.3.*](http://projects.spring.io/spring-boot/) 
  * Since 1.1.2 also Spring Boot 1.4.*+ is supported 
* [Admin LTE](https://almsaeedstudio.com/preview)
* [Thymeleaf](http://www.thymeleaf.org/)

## Explore
1. Download the project
2. execute a `mvn clean install` or import to IDE
3. run the Demo JAR
4. go to localhost:8090/ or localhost:8090/admintool

 -> See [Spring Boot Demo application](admin-tools-demo/)

## Usage

### Dependency and Configuration
Include the dependencies in your dependency management. You can find it in [Maven Central](https://mvnrepository.com/artifact/de.chandre.admin-tools).
```xml

<dependency>
	<groupId>de.chandre.admin-tools</groupId>
	<artifactId>admin-tools-core</artifactId>
	<version>1.1.6.5</version>
</dependency>
	...
```

To get components scanned add the package to @ComponentScan
```java

//required in your own application to get the admintool scanned  
@ComponentScan(basePackages={"de.chandre.admintool"})

```

### Adding own Pages

#### Creating an AdminComponent 
The AdminComponent is the main component for configuring a module. It must contain a menu entry. Furthermore you can append custom CSS and JS (with either relative or absolute URLs) to components, which will only be resolved within component calls.
```java

//create a new component ... since 1.1.6 it's possible to use chained builders
AdminComponent component = AdminComponentImpl.builder()
		.displayName("Demo-App-Component")
		.notificationTemplate("notifications/notification")
		.securityRole("ROLE_ANONYMOUS")
		.securityRole("ROLE_ADMIN")
		.position(1).build();

//adding a custom (relative) js
component.addAdditionalJS("/static/mycomponent/js/myJavaScript.js", true);

//adding a custom (relative) css
component.addAdditionalCSS("/static/mycomponent/css/myCascadingStyleSheet.css", true);

MenuEntry mainMenu = new MenuEntry();
mainMenu.setDisplayName("Demo-App-Component");
mainMenu.setName("demo");
mainMenu.setTarget("content/mycomponent");
mainMenu.addVariable("message", "Welcome to your Dashboard");
component.setMainMenu(mainMenu);

```

Each menu entry can have sub menu entries. Because of the AdminLTE implementation a menu entry with an existing sub menu will not be displayed anymore (the target would be useless). 
```java

MenuEntry mainMenu = new MenuEntry();
mainMenu.setDisplayName("Demo-App-Component");
mainMenu.setName("demo");
mainMenu.setTarget("");
mainMenu.addVariable("message", "Welcome to your Dashboard");
component.setMainMenu(mainMenu);

//adding sub menu entries with
// unique name, display name, template target
mainMenu.addSubmenuEntry(new MenuEntry("dashboard", "Dashboard", "content/dashboard"));
mainMenu.addSubmenuEntry(new MenuEntry("dashboard2", "Dashboard 2", "content/dashboard2"));
	
//adding a new sub menu tree ... since 1.1.6 it's possible to use chained builders
MenuEntry submenu = MenuEntry.builder().displayName("SubMulti")
		.resouceMessageKeySuffix("demo.subMulti.displayName")
		.submenuEntry(
				MenuEntry.builder().name("dashboard3")
					.displayName("Dashboard 3").target("content/dashboard3")
					.resouceMessageKeySuffix("demo.subMulti.dashboard3.displayName").build())
		.build();
submenu.addSubmenuEntry(
		MenuEntry.builder().name("dashboard4").displayName("Dashboard 4")
			.target("content/dashboard4")
			.resouceMessageKeySuffix("demo.subMulti.dashboard4.displayName").build());
mainMenu.addSubmenuEntry(submenu);

//since 1.0.3 it's possible to add custom js and css on menuEntry
submenu.addAdditionalJS("/static/mycomponent/js/myMenuJavaScript.js", true);
submenu.addAdditionalCSS("/static/mycomponent/css/myMenuCascadingStyleSheet.css", true);

mainMenu.addSubmenuEntry(submenu);
	
```
the code above will create the following menu structure
```
-- Demo-App-Component
    -- Dashboard
    -- Dashboard 2
    -- SubMulti
        -- Dashboard 3
        -- Dashboard 4
```

Template target resolvement:
E.g. your Thymeleaf is configured to look for templates in: *classpath:/templates*
* than your template has to be in: */templates/admintool/*
* you want to use your own structure: */templates/admintool/myComponent/myMenuTemplate.html*
* to get the example resolved set target to: *myComponent/myMenuTemplate*

#### Creating a Content Template

##### Before version 1.1.0 (Deprecated)
A content template must at least contain a block element with *id="template-content"*, otherwise the content will not be found by Thymeleaf. Using the namespace within the HTML tag provides code completion in Eclipse IDE with installed Thymeleaf plugin.   
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

The reason for this special id (*id="template-content"*) is that the index.html within the core module will include the content-page content through this particular id `<th:block th:include="${contentPage} :: #template-content" />`. So all other HTML tags outside the block element with this special id will be ignored.

##### Since version 1.1.0
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

This way it will be much easier to apply a custom layout.

### Template Resolution 
The Thymeleaf templates will be fetched from the *admintool* folder of the configured template folder.
e.g. if configured Thymeleaf root template folder is *templates* all custom (admintool-)templates should be placed within: 
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


The *MenuEntry.target* should point relative from the *admintool* folder to the template which should be used by this menu entry. This template will be shown in the main frame (within tag `<div class="content-wrapper">`).

You can also override templates provided by the admin-tools-core library. Per default templates will be found by *OrderedClassLoaderResourceResolver* using the *TemplateUrlComparator*. If more than one template has been found the core template will be used last, all others in natural string comparison order of absolute template URL (with JAR names). The first will be picked. -> Your desired template should be named to lead the comparison order.

### Enable Internationalization
To enable set configuration `admintool.core.internationalizationEnabled=true`.

If you enable internationalization feature you must set the ResouceMessageKey on each MenuEntry, otherwise you will get an exception while rendering the menu (java.lang.IllegalArgumentException: Message key cannot be null).

Furthermore you have load all the message resources. Unfortunately Spring doesn't allow configuration via wildcard:
```java
@Bean
public ReloadableResourceBundleMessageSource messageSource() {
	ReloadableResourceBundleMessageSource messageSource = new ReloadableResourceBundleMessageSource();
	messageSource.setBasenames(
			"classpath:i18n/admintool/core-messages",
			"classpath:i18n/admintool/security-messages",
			"classpath:i18n/admintool/dbbrowser-messages",
			"classpath:i18n/admintool/filebrowser-messages",
			"classpath:i18n/admintool/jmx-messages",
			"classpath:i18n/admintool/log4j2-messages",
			"classpath:i18n/admintool/melody-messages",
			"classpath:i18n/admintool/properties-messages",
			"classpath:i18n/admintool/quartz-messages",
			"classpath:i18n/admintool/demo-messages");
	messageSource.setDefaultEncoding("UTF-8");
	return messageSource;
}

@Bean
public LocaleResolver localeResolver() {
	SessionLocaleResolver slr = new SessionLocaleResolver();
	slr.setDefaultLocale(Locale.ENGLISH);
	return slr;
}
```

### Checking the Menu Integrity
There are two options to do that. First will be including the menu integrity check template (introduced with 1.1.3) anywhere (But for execution you have to call the page).
```html
<th:block th:include="admintool/includes/integrityCheck.inc" />
```
The second option would be calling the method directly, but of couse after spring has finished loading its context.
```java

@Bean
public ApplicationListener<ContextRefreshedEvent> contextLoadedListener(AdminToolIntegrityUtil integrityUtil) {
	return new ApplicationListener<ContextRefreshedEvent>() {
		@Override
		public void onApplicationEvent(ContextRefreshedEvent event) {
			//checking the menu integrity
			integrityUtil.checkMenuIntegrityAndPrintLog();
		}
	};
}

```

### Packing the Core-Menu-Structure

Packing the menu structure to creating only one (core-)component and adding all core-components to this single one.

Until version 1.1.3 the flattening is not really recommended, because all CSS and JS should be appended to the new component.
```java

/*
 * restructure the core components
 */
AdminComponent component3 = new AdminComponentImpl();
component3.setPosition(4);
component3.setDisplayName("App-Management");

MenuEntry appMenu = new MenuEntry();
appMenu.setDisplayName("App-Management");
appMenu.setName("appManagement");
for (AdminComponent rootComponent : adminTool.getComponents()) {
	component3.getSecurityRoles().addAll(rootComponent.getSecurityRoles());
	rootComponent.getMainMenu().getSecurityRoles().addAll(rootComponent.getSecurityRoles());
	
	//add the existing css and js entries to new component
	component3.getAdditionalCSS().putAll(rootComponent.getAdditionalCSS());
	component3.getAdditionalJS().putAll(rootComponent.getAdditionalJS());
	
	appMenu.addSubmenuEntry(rootComponent.getMainMenu());
}
component3.setMainMenu(appMenu);

//clear all components
adminTool.getComponents().clear();

//finally add the new component
adminTool.getComponents().add(component3);

```

Since version 1.1.4 some new features make it possible to add additional CSS and JS to the MenuEntry and resolve it backward recursive to the root menu

```java

/*
 * restructure the core components
 */
AdminComponent component3 = new AdminComponentImpl();
component3.setPosition(4);
component3.setDisplayName("App-Management");

MenuEntry appMenu = new MenuEntry();
appMenu.setDisplayName("App-Management");
appMenu.setName("appManagement");
for (AdminComponent rootComponent : adminTool.getComponents()) {
	component3.getSecurityRoles().addAll(rootComponent.getSecurityRoles());
	rootComponent.getMainMenu().getSecurityRoles().addAll(rootComponent.getSecurityRoles());
	
	//because of loosing the component, copy additional css and js to its main menu entry, otherwise each menu will load everything
	rootComponent.getMainMenu().getAdditionalCSS().putAll(rootComponent.getAdditionalCSS());
	rootComponent.getMainMenu().getAdditionalJS().putAll(rootComponent.getAdditionalJS());
	
	//tell all menu entries using the hierarchy for loading css and js
	rootComponent.getMainMenu().flattened().forEach(menu -> {
		menu.setUseCCSHierarchy(true);
		menu.setUseJSHierarchy(true);
	});
	
	appMenu.addSubmenuEntry(rootComponent.getMainMenu());
}
component3.setMainMenu(appMenu);

//clear all components
adminTool.getComponents().clear();

//finally add the new component
adminTool.getComponents().add(component2);
		
```

### Using AdminTool Core JS 

#### Extending the AdminTool.Core

The admin-tool-core artifact has a small javascript which could be extended to benefit from the predefined functions. It depends on JQuery (plugin).

```javascript
    
// creating a new Function
AdminTool.MyComponent = function(el, options) {
	if (el) {
		this.init(el, options)
	}
}

//extending the Core
AdminTool.MyComponent.prototype = new AdminTool.Core();

//
$.extend(AdminTool.MyComponent.prototype, {
	name : 'myComponent',

	postInit: function() {
		//postInit will be called automatically after plug-in creation
		this.myVar = 'myVar';
	},
	
    // function which will be called while destroy() is called
    unbind : function() {
		//unbind your variables
    },

	myCustomFunction: function() {
		return this.myVar;
	},
	
	myCustomClick: function() {
		// getByID - common function of adminTool.js (also: getByClazz()). will return the JQuery wrapped element
		// to call a function in same context within a $.on method you have to use $.proxy 
		getByID('saveObject').on('click', $.proxy(this.saveObjectConfirm, this, false));
		/* 
		 * // alternatively you can use the plugin directly, 
		 * // maybe if you are within a $.each() loop where "this" is not the plugin
		 * 
		 * button.click(function() {
		 *     $("#objects").users('saveObjectConfirm', false);
		 * });
		 */
	},
	
	saveObjectConfirm: function(bool) {
		// showing a default confirm modal (1.1.5) with a confirm function
		this.showConfirmModal("Save Object", "Do you really want to save this object?", this.saveObject, bool);
	},
	
	saveObject: function(bool) {
		// the following sendRequest method (from AdminTool.Core) will automatically set the CSRF token.
		// Only "url" is required. All other parameters are optional. Default is a GET.
		// To access the own plugin context you should transfer it with the parameters (in this case: ctx) 
		this.sendRequest({
			url: '/myUri', 
			requestType: "POST", 
			dataType: "json", 
			data: JSON.stringify(userData),
			showModalOnError: true,
			ctx: this
		},
		function(data, query) {
			// result object data depends (of course) on your implementation
			if (data && data == 'true') {
				// doing something usefull. access the own context with the  
				// access the the context wit ctx				
				query.ctx.usefullEnding(data);
			} else {
				//show error modal (AdminTool.Core)
				query.ctx.showErrorModal('Error saving User', data);
			}
		});
	},
	
	usefullEnding: function(data) {
		
	}
	
});

// create the plugin
$.pluginMaker(AdminTool.MyComponent);

//load the plugin
$( document ).ready(function() {
	if ($("#objects").length > 0) {
		// initialize the plugin and bind it to element with id #objects
		// AdminTool.MyComponent.prototype.name will be used
		$("#objects").myComponent();
	}
});
```

#### Methods of AdminTool.Core JS
```javascript
	
/** *************************************
 * Object / JQuery extensions
 * ************************************* */

//Extension "startsWith" for String objects
var boolean = "myString".startsWith(String);

/**
 * removes one css class and adds the other
 * @param classToRemove String
 * @param classToAdd String
 */
$myQueryObj.removeAddClass(classToRemove, classToAdd);

/**
 * switches the css classes on the object
 * @param classToRemove
 * @param classToAdd
 */
$myQueryObj.switchClass(classToCheck1, classToCheck2);


/** *************************************
 * global functions
 * ************************************* */

// returns the web application context 
var String = getWebContext();

// returns the CSRF token
var String = getCSRFToken();

// returns the CSRF header name
var String = getCSRFHeader();
 
// retuns a string with '#' as first char
var String = getID(id);

// returns the JQuery element found by ID
var $element = getByID(id);

// retuns a string with '.' as first char
var String = getClazz(clazz);

// returns the JQuery elements found by Class
var $elements = getByClazz(clazz);

/**
 * Sends a Ajax Request with CSRF header token.
 *  
 * @param serviceUrl - (required) URI to call (Web-Application context will be set automatically)
 * @param requestType - (required) GET / POST ...
 * @param dataType - (required) json / text ...
 * @param callback(responseData) - (required)  callback function with one parameter
 */
sendRequest(serviceUrl, requestType, dataType, callback);


/** *************************************
 * following methods could be called with "this" if it's called within context of an extended AdminTool.Core plugin
 * ************************************* */

//reloads the page
this.reloadPage();

/**
 * Sends a Ajax Request with CSRF header token. Per default it's a GET.
 * 
 *  @param query - the query object
 *  @param query.url - (required) URI to call (Web-Application context will be set automatically)
 *  @param query.dataType - (optional) Default: json
 *  @param query.requestType - (optional) Default: GET
 *  @param query.data - (optional) Default: null
 *  @param query.contentType - (optional) Default: application/json; charset=UTF-8
 *  @param query.showModalOnError - (optional) if true the default error modal with optional message will be shown
 *  @param query.erroModalHeadline - (optional) headline for error modal
 *  @param query.errorModalText - (optional) text for error modal
 *  
 *  @param callback(data, query) - callback function with two parameters.first is the data, second the original query param.
 */
this.sendRequest(query, callbackFunction);

/**
 * shows the default error modal which will be identified by "this.options.errorModalId"
 * 
 * @param headline - headline for error modal (optional)
 * @param text - text for error modal (optional)
 */
this.showErrorModal(headline, text);

/**
 * shows the default confirm modal
 * 
 * @param confirmTitle - headline for confirm modal (optional)
 * @param confirmMessage - text for confirm modal (optional)
 * @param confirmCallback - a callback function for on.click "this.options.confirmModalButtonId"
 * @param args - arguments for callback (optional)
 */
this.showConfirmModal(confirmTitle, confirmMessage, confirmCallback, args);
```