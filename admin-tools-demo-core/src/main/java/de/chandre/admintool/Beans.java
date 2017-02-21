package de.chandre.admintool;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Collection;
import java.util.List;
import java.util.Locale;

import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.sql.DataSource;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.Appender;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.appender.db.jdbc.ColumnConfig;
import org.apache.logging.log4j.core.appender.db.jdbc.ConnectionSource;
import org.apache.logging.log4j.core.appender.db.jdbc.JdbcAppender;
import org.apache.logging.log4j.core.config.Configuration;
import org.apache.logging.log4j.core.config.LoggerConfig;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.EnableMBeanExport;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.jmx.export.MBeanExporter;
import org.springframework.jmx.export.assembler.SimpleReflectiveMBeanInfoAssembler;
import org.springframework.web.servlet.LocaleResolver;
import org.springframework.web.servlet.i18n.LocaleChangeInterceptor;
import org.springframework.web.servlet.i18n.SessionLocaleResolver;

import de.chandre.admintool.core.AdminToolConfig;
import de.chandre.admintool.core.utils.AdminToolIntegrityUtil;
import de.chandre.admintool.log4j2.AdminToolLog4j2Util;

@org.springframework.context.annotation.Configuration
@EnableMBeanExport
public class Beans {
	
	private static final Logger LOGGER = LogManager.getFormatterLogger(Beans.class);
	
	@Bean
    public LocaleResolver localeResolver() {
        SessionLocaleResolver slr = new SessionLocaleResolver();
        slr.setDefaultLocale(Locale.GERMAN);
        return slr;
    }
	
	@Bean
	public LocaleChangeInterceptor localeChangeInterceptor() {
	    LocaleChangeInterceptor lci = new LocaleChangeInterceptor();
	    lci.setParamName("lang");
	    return lci;
	}
	
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
	
	/**
	 * maybe not reasonable to export all configuration beans as mbeans, especially configurations like fileBrowser or dbBrowser
	 * 
	 * @param mbeanExporter
	 * @param configs
	 * @return
	 * @throws MalformedObjectNameException
	 */
	@Bean
	public boolean adminToolMbeansExported(MBeanExporter mbeanExporter, List<AdminToolConfig> configs) throws MalformedObjectNameException {
		
		SimpleReflectiveMBeanInfoAssembler assembler = new SimpleReflectiveMBeanInfoAssembler();
		mbeanExporter.setAssembler(assembler);
		mbeanExporter.setAutodetect(true);
		LOGGER.info("registering %s managed admintool config resources", configs.size());
		for (AdminToolConfig adminToolConfig : configs) {
			LOGGER.info("registering managed resource: %s", adminToolConfig.getClass().getName());
			ObjectName name = new ObjectName(String.format("de.chandre.admintool:type=Config,name=%s", adminToolConfig.getClass().getSimpleName()));
			mbeanExporter.registerManagedResource(adminToolConfig, name);
		}
		return true;
	}
	
	/**
	 * 
	 * 
	 * @param dataSource
	 * @return
	 * @see
	 * https://logging.apache.org/log4j/2.x/manual/appenders.html#JDBCAppender
	 * http://stackoverflow.com/questions/17593308/how-to-use-spring-bonecpdatasource-bean-as-data-source-for-log4j-2-jdbc-appender
	 */
	@Bean 
	public Appender datasourceAppender(DataSource dataSource, AdminToolLog4j2Util logUtil) {
		LOGGER.info("adding jdbc appender");
		
		final LoggerContext ctx = (LoggerContext) LogManager.getContext(false);
		final Configuration config = ctx.getConfiguration();
		
		ColumnConfig[] cc = {
				ColumnConfig.createColumnConfig(config, "DATE", null, null, "true", null, null),
	            ColumnConfig.createColumnConfig(config, "LEVEL", "%level", null, null, null, null),
	            ColumnConfig.createColumnConfig(config, "LOGGER", "%logger", null, null, null, null),
	            ColumnConfig.createColumnConfig(config, "MESSAGE", "%message", null, null, null, "true"),
	            ColumnConfig.createColumnConfig(config, "EXCEPTION", "%ex{full}", null, null, null, "true"),
		};
		
		Appender appender = JdbcAppender.createAppender("databaseAppender", "false", null, new Connect(dataSource), "0", "LOGGING", cc);
		appender.start();
		config.addAppender(appender);
		LoggerConfig loggerConfig = config.getLoggerConfig(LogManager.ROOT_LOGGER_NAME);
		loggerConfig.addAppender(appender, Level.INFO, null);
		
		Collection<org.apache.logging.log4j.core.Logger> loggers = logUtil.getParentLoggers();
		for (Logger logger : loggers) {
			LoggerConfig parLoggerConfig = config.getLoggerConfig(logger.getName());
			if (logger.getName().contains("chandre")) {
				parLoggerConfig.addAppender(appender, Level.DEBUG, null);
			}
			else {
				parLoggerConfig.addAppender(appender, Level.INFO, null);
			}
		}
		ctx.updateLoggers();
		
		return appender;
	}
	
	// inner class
	class Connect implements ConnectionSource {
		private DataSource dsource;

		public Connect(DataSource dsource) {
			this.dsource = dsource;
		}

		@Override
		public Connection getConnection() throws SQLException {
			return this.dsource.getConnection();
		}
	}
}
