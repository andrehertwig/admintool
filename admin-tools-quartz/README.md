# The Quartz-Scheduler Management Plugin
> View, edit, start Quartz-Jobs / Triggers ... 

![Preview image](doc/screen_quartzMetadata_org.png?raw=true "AdminTool Quartz Metadata UI")
![Preview image](doc/screen_quartzJobs_org.png?raw=true "AdminTool Quartz Jobs UI")

## Features
* `Scheduler`: starting, stopping the scheduler instance
* `Jobs`: edit, interrupt, start jobs, add triggers
* `Triggers`:  pause, resume, edit, interrupt, remove triggers

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
		<version>1.1.3</version>
	</dependency>
	<dependency>
		<groupId>de.chandre.admin-tools</groupId>
		<artifactId>admin-tools-quartz</artifactId>
		<version>1.1.3</version>
	</dependency>
	
```

## Configurations
_since 1.0.1_

```ini

	# to deactivate the admin component at all
	admintool.quartz.enabled=true
	
	# to hide the menu item in GUI, but still callable
	admintool.quartz.hideMenuItem=false
	
	# to deactivate stopping the scheduler
	admintool.quartz.stopSchedulerAllowed=true
	
	# to deactivate executing any job
	admintool.quartz.executeJobAllowed=true
	
	# to deactivate interrupting any job
	admintool.quartz.interruptJobAllowed=true
	
	# to deactivate changing any job information
	admintool.quartz.changeJobInfoAllowed=true
	
	# to deactivate changing any trigger information 
	admintool.quartz.changeTriggerAllowed=true
	
	# to deactivate changing the trigger state (paused, running)
	admintool.quartz.changetTriggerStateAllowed=true
	
	# to deactivate interrupting any trigger
	admintool.quartz.interruptTriggerAllowed=true
	
	# to deactivate adding triggers to a job
	admintool.quartz.addTriggerAllowed=true
	
	# to deactivate removing triggers
	admintool.quartz.removeTriggerAllowed=true
	
	#since 1.0.1
	# for own implementation or requires admin-tools-core-security
	#semi-colon separated list of Spring Security roles like ROLE_ANONYMOUS;ROLE_ADMIN
	admintool.quartz.securityRoles=
	
	#since 1.0.1
	# integer value. used by default comparator to order components
	admintool.quartz.componentPosition=
	
```