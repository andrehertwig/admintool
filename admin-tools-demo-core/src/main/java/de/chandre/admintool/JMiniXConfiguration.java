package de.chandre.admintool;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jminix.console.servlet.MiniConsoleServlet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.embedded.ServletRegistrationBean;
import org.springframework.boot.context.web.SpringBootServletInitializer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

/**
 * Servlet initializer for JMiniX (JMX) Servlet
 *  
 * @author adhr
 *
 */
@Configuration
public class JMiniXConfiguration extends SpringBootServletInitializer
{
	private static final Logger LOGGER = LogManager.getFormatterLogger(JMiniXConfiguration.class);
	
	@Autowired
	private Environment environment;
	
	@Bean
    public MiniConsoleServlet jMiniXServlet() {
        return new MiniConsoleServlet();
    }
	
	@Bean
	public ServletRegistrationBean jMiniXServletRegistration(@Value("${jminix.urlMappings:/jmx/*}") String urlMappings) {
		ServletRegistrationBean registration = new ServletRegistrationBean(jMiniXServlet());
		
		if (null == urlMappings || urlMappings.startsWith("$")) {
			if (null != environment && null != environment.getProperty("jminix.urlMappings")) {
				urlMappings = environment.getProperty("jminix.urlMappings");
			} else {
				urlMappings = "/jmx/*";
			}
		}
		
		
		LOGGER.info("initalize JMX-Servlet with URL mapping: %s", urlMappings);
		for (String urlMapping : urlMappings.split(";")) {
			if (StringUtils.isNotBlank(urlMapping)) {
				registration.addUrlMappings(urlMapping);
			}
		}
		return registration;
	}
}
