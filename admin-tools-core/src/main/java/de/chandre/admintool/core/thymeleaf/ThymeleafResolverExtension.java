package de.chandre.admintool.core.thymeleaf;

import java.nio.charset.Charset;

import javax.annotation.PostConstruct;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.thymeleaf.spring4.SpringTemplateEngine;

/**
 * Configuration for custom template resolver using the spring configuration attributes 
 * but with highest order in time of creation 
 * 
 * @author Andre
 * @since 1.0.1
 */
@Configuration
public class ThymeleafResolverExtension {
	
	private static final Log LOGGER = LogFactory.getLog(ThymeleafResolverExtension.class);
	
	public static final String DEFAULT_PREFIX = "classpath:/templates/";
	public static final String DEFAULT_SUFFIX = ".html";
	
	@Autowired
    private SpringTemplateEngine templateEngine;
	
	@Value("${spring.thymeleaf.prefix:#{T(de.chandre.admintool.core.thymeleaf.ThymeleafResolverExtension).DEFAULT_PREFIX}}")
	private String prefix;
	
	@Value("${spring.thymeleaf.suffix:#{T(de.chandre.admintool.core.thymeleaf.ThymeleafResolverExtension).DEFAULT_SUFFIX}}")
	private String suffix;
	
	@Value("${spring.thymeleaf.mode:HTML5}")
	private String mode;
	
	@Value("#{T(java.nio.charset.Charset).forName('${spring.thymeleaf.encoding:UTF-8}')}")
	private Charset encoding;
	
	@Value("${spring.thymeleaf.cache:true}")
	private boolean cache;
	
	@PostConstruct
    public void extension() {
		
		LOGGER.info(toString());
		
		OrderedClassLoaderTemplateResolver resolver = new OrderedClassLoaderTemplateResolver();
		resolver.setOrder(templateEngine.getTemplateResolvers().size());
		resolver.setPrefix(prefix);
		resolver.setSuffix(suffix);
		resolver.setTemplateMode(mode);
		if (encoding != null) {
			resolver.setCharacterEncoding(encoding.name());
		}
		resolver.setCacheable(cache);
		templateEngine.addTemplateResolver(resolver);
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("ThymeleafResolverExtension [prefix=").append(prefix).append(", suffix=").append(suffix)
				.append(", mode=").append(mode).append(", encoding=").append(encoding).append(", cache=").append(cache)
				.append("]");
		return builder.toString();
	}
	
	
}

