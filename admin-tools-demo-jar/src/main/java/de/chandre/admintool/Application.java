package de.chandre.admintool;

import java.util.Arrays;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.web.servlet.ServletComponentScan;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.EnableMBeanExport;

/**
 * The Spring Boot starter class
 * @author adhr
 *
 */
@SpringBootApplication
@EnableAutoConfiguration
@EnableConfigurationProperties
@ServletComponentScan(basePackages={"de.chandre.admintool"})
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

}
