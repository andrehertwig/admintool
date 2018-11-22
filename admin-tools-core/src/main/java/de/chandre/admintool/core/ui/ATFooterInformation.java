package de.chandre.admintool.core.ui;

public interface ATFooterInformation {

	default String getApplicationName() {
		return "The Admin-UI";
	};

	String getAppVersionDescription();

	String getAppVersion();

	default String getAppBuildTimeDescritpion() {
		return "Build-Time";
	}

	String getAppBuildTime();
}
