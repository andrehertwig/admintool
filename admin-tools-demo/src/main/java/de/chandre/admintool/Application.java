package de.chandre.admintool;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Locale;

import javax.sql.DataSource;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.core.Appender;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.appender.db.jdbc.ColumnConfig;
import org.apache.logging.log4j.core.appender.db.jdbc.ConnectionSource;
import org.apache.logging.log4j.core.appender.db.jdbc.JdbcAppender;
import org.apache.logging.log4j.core.config.Configuration;
import org.apache.logging.log4j.core.config.LoggerConfig;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.servlet.ServletComponentScan;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.EnableMBeanExport;
import org.springframework.web.servlet.LocaleResolver;
import org.springframework.web.servlet.i18n.LocaleChangeInterceptor;
import org.springframework.web.servlet.i18n.SessionLocaleResolver;

import de.chandre.admintool.log4j2.AdminToolLog4j2Util;

/**
 * The Spring Boot starter class
 * @author adhr
 *
 */
@SpringBootApplication
@ServletComponentScan
@EnableMBeanExport
//required in own application to get admintool scanned  
@ComponentScan(basePackages={"de.chandre.admintool"})
public class Application
{
	private static final Logger LOGGER = LogManager.getFormatterLogger(Application.class);

	public static void main(String[] args)
	{
		LOGGER.info("starting admin tool boot app");
		ApplicationContext ctx = SpringApplication.run(Application.class, args);
		
		if (LOGGER.isDebugEnabled()) {
			String[] beanNames = ctx.getBeanDefinitionNames();
	        Arrays.sort(beanNames);
	        for (String beanName : beanNames) {
	        	LOGGER.trace(beanName);
	        }
		}
	}
	
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
