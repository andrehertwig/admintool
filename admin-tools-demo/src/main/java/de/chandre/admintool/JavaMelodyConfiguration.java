package de.chandre.admintool;

import javax.servlet.DispatcherType;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.aop.framework.autoproxy.DefaultAdvisorAutoProxyCreator;
import org.springframework.aop.support.annotation.AnnotationMatchingPointcut;
import org.springframework.boot.context.embedded.FilterRegistrationBean;
import org.springframework.boot.context.embedded.ServletContextInitializer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.RestController;

import net.bull.javamelody.MonitoredWithAnnotationPointcut;
import net.bull.javamelody.MonitoringFilter;
import net.bull.javamelody.MonitoringSpringAdvisor;
import net.bull.javamelody.SessionListener;
import net.bull.javamelody.SpringDataSourceBeanPostProcessor;

/**
 * 
 * @see https://github.com/javamelody/javamelody/wiki/UserGuideAdvanced#spring-boot-app
 * @author speralta, evernat
 */
@Configuration
@ImportResource("classpath:net/bull/javamelody/monitoring-spring.xml")
@SuppressWarnings("javadoc")
public class JavaMelodyConfiguration implements ServletContextInitializer
{
	private static final Logger LOGGER = LogManager.getFormatterLogger(JavaMelodyConfiguration.class);
	
	@Override
	public void onStartup(ServletContext servletContext) throws ServletException {
		servletContext.addListener(new SessionListener());
	}

	@Bean
	public FilterRegistrationBean javaMelody() {
		FilterRegistrationBean javaMelody = new FilterRegistrationBean();
		javaMelody.setFilter(new MonitoringFilter());
		javaMelody.setAsyncSupported(true);
		javaMelody.setName("javamelody");
		javaMelody.setDispatcherTypes(DispatcherType.REQUEST, DispatcherType.ASYNC);

		// see the list of parameters:
		// https://github.com/javamelody/javamelody/wiki/UserGuide#6-optional-parameters
		//javaMelody.addInitParameter(Parameter.LOG.getCode(), Boolean.toString(true));
		// to add basic auth:
		// javaMelody.addInitParameter(Parameter.AUTHORIZED_USERS.getCode(), "admin:pwd");
		// to change the default storage directory:
		// javaMelody.addInitParameter(Parameter.STORAGE_DIRECTORY.getCode(), "/tmp/javamelody");

		javaMelody.addUrlPatterns("/*");
		return javaMelody;
	}
	
	// Note: if you have auto-proxy issues, you can add the following dependency in your pom.xml:
		// <dependency>
		//   <groupId>org.aspectj</groupId>
		//   <artifactId>aspectjweaver</artifactId>
		// </dependency> 
		@Bean
		public DefaultAdvisorAutoProxyCreator getDefaultAdvisorAutoProxyCreator() {
			return new DefaultAdvisorAutoProxyCreator();
		}

		// monitoring of jdbc datasources:
		@Bean
		public SpringDataSourceBeanPostProcessor monitoringDataSourceBeanPostProcessor() {
			final SpringDataSourceBeanPostProcessor processor = new SpringDataSourceBeanPostProcessor();
			processor.setExcludedDatasources(null);
			return processor;
		}

		// monitoring of beans or methods having @MonitoredWithSpring:
		@Bean
		public MonitoringSpringAdvisor monitoringAdvisor() {
			final MonitoringSpringAdvisor interceptor = new MonitoringSpringAdvisor();
			interceptor.setPointcut(new MonitoredWithAnnotationPointcut());
			return interceptor;
		}

		// monitoring of all services and controllers (even without having @MonitoredWithSpring):
		@Bean
		public MonitoringSpringAdvisor springServiceMonitoringAdvisor() {
			final MonitoringSpringAdvisor interceptor = new MonitoringSpringAdvisor();
			interceptor.setPointcut(new AnnotationMatchingPointcut(Service.class));
			return interceptor;
		}

		@Bean
		public MonitoringSpringAdvisor springControllerMonitoringAdvisor() {
			final MonitoringSpringAdvisor interceptor = new MonitoringSpringAdvisor();
			interceptor.setPointcut(new AnnotationMatchingPointcut(Controller.class));
			return interceptor;
		}

		@Bean
		public MonitoringSpringAdvisor springRestControllerMonitoringAdvisor() {
			final MonitoringSpringAdvisor interceptor = new MonitoringSpringAdvisor();
			interceptor.setPointcut(new AnnotationMatchingPointcut(RestController.class));
			return interceptor;
	}
}
