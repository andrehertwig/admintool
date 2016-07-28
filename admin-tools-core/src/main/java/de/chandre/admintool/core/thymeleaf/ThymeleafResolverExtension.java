package de.chandre.admintool.core.thymeleaf;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.thymeleaf.ThymeleafProperties;
import org.springframework.context.annotation.Configuration;
import org.thymeleaf.spring4.SpringTemplateEngine;

/**
 * Configuration for custom template resolver using the spring configuration attributes but with highest order in time of creation 
 * @author Andre
 * @since 1.0.1
 */
@Configuration
public class ThymeleafResolverExtension {
	
	@Autowired
    private SpringTemplateEngine templateEngine;
	
	@Autowired
	private ThymeleafProperties properties;
	
	@PostConstruct
    public void extension() {
		OrderedClassLoaderTemplateResolver resolver = new OrderedClassLoaderTemplateResolver();
		resolver.setOrder(templateEngine.getTemplateResolvers().size());
		resolver.setPrefix(this.properties.getPrefix());
		resolver.setSuffix(this.properties.getSuffix());
		resolver.setTemplateMode(this.properties.getMode());
		if (this.properties.getEncoding() != null) {
			resolver.setCharacterEncoding(this.properties.getEncoding().name());
		}
		resolver.setCacheable(this.properties.isCache());
		templateEngine.addTemplateResolver(resolver);
	}
}
