package de.chandre.admintool.core;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;


@Configuration
//@AutoConfigureAfter(DispatcherServletAutoConfiguration.class)
//@ConditionalOnClass({ WebMvcAutoConfiguration.class })
public class CustomWebMvcAutoConfig extends WebMvcConfigurerAdapter 
{
	@Override
	public void addResourceHandlers(ResourceHandlerRegistry registry) {
		if (!registry.hasMappingForPattern("/static/**")) {
			registry.addResourceHandler("/static/**").addResourceLocations("classpath:/static/");
		}
		if (!registry.hasMappingForPattern("/webjars/**")) {
			registry.addResourceHandler("/webjars/**").addResourceLocations("classpath:/META-INF/resources/webjars/");
		}
		
		super.addResourceHandlers(registry);
	}
	
	
}
