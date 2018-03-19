# The Properties visualization Plugin
> Shows Git Properties if in classpath and Spring environment properties

 ![Preview image](doc/screen_propertyBrowser_org.png?raw=true "AdminTool Properties UI")

## Introduced with
* admin-tools-core:1.0.1

## Usage

```xml
<dependency>
	<groupId>de.chandre.admin-tools</groupId>
	<artifactId>admin-tools-core</artifactId>
	<version>1.1.6.2</version>
</dependency>
<dependency>
	<groupId>de.chandre.admin-tools</groupId>
	<artifactId>admin-tools-properties</artifactId>
	<version>1.1.6.2</version>
</dependency>
```

To create the git.properties file easily you can use for example the [GIT Commit-Id Plugin](https://github.com/ktoso/maven-git-commit-id-plugin).

```xml
<plugin>
	<!-- https://github.com/ktoso/maven-git-commit-id-plugin -->
	<groupId>pl.project13.maven</groupId>
	<artifactId>git-commit-id-plugin</artifactId>
	<version>2.2.3</version>
	<executions>
		<execution>
			<goals>
				<goal>revision</goal>
			</goals>
		</execution>
	</executions>
	<configuration>
		<verbose>true</verbose>
		<dateFormat>yyyy-MM-dd'T'HH:mm:ssZ</dateFormat>
		<generateGitPropertiesFile>true</generateGitPropertiesFile>
		<generateGitPropertiesFilename>${project.build.outputDirectory}/git.properties</generateGitPropertiesFilename>
	</configuration>
</plugin>
```

## Configurations

Following the default values are shown.	
```ini

#since 1.0.3
# to deactivate the admin component
adminTool.properties.enabled=true

# path to git.properties
admintool.properties.gitPropertiesPath=classpath:git.properties

# encoding o git.properties
admintool.properties.gitPropertiesEncoding=UTF-8

# since 1.1.6
# option to disable viewing the environment properties
adminTool.properties.showEnvironmentProperties=true

# for own implementation or requires admin-tools-core-security
#semi-colon separated list of Spring Security roles like ROLE_ANONYMOUS;ROLE_ADMIN
admintool.properties.securityRoles=

# integer value. used by default comparator to order components
admintool.properties.componentPosition=

# since 1.1.4
# semicolon separated list of additional properties. Spring resource loading will be used.
# paths should start either with "classpath:/" or "file:/"
admintool.properties.addPropertiesPaths=
	
```