package de.chandre.admintool;

import java.util.Arrays;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.support.SpringBootServletInitializer;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

/**
 * The Spring Boot starter class
 * @author adhr
 *
 */
@Configuration
@EnableAutoConfiguration
//required in own application to get admintool scanned  
@ComponentScan(basePackages={"de.chandre.admintool"})
public class WarApplication extends SpringBootServletInitializer
{
	private static final Logger LOGGER = LogManager.getFormatterLogger(WarApplication.class);
	
	@Override
    protected SpringApplicationBuilder configure(SpringApplicationBuilder application) {
        return application.sources(Application.class);
    }

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
