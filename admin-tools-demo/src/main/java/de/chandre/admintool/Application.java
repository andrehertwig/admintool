package de.chandre.admintool;

import java.util.Arrays;
import java.util.Locale;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
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

}
