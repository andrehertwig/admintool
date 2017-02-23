package de.chandre.admintool.core.thymeleaf;

import java.util.Comparator;

import org.thymeleaf.templateresolver.TemplateResolver;

/**
 * Custom template resolver used to get the correct template
 * <br>
 * If using the default constructor the {@link TemplateUrlComparator} will be used to sort 
 * the templates within {@link OrderedClassLoaderResourceResolver} after searching them in the classpath. <br>
 * If more than one template has been found, the first one by compared set will be used. 
 *  
 * @author Andre
 * @since 1.0.1
 */
public class OrderedClassLoaderTemplateResolver extends TemplateResolver {

	private final OrderedClassLoaderResourceResolver resourceResolver;
	
	/**
	 * constructor using {@link TemplateUrlComparator} and enabled caching
	 */
	public OrderedClassLoaderTemplateResolver() {
		 this(new TemplateUrlComparator());
	}

	/**
	 * constructor with custom comparator and enabled caching
	 * @param comparator
	 */
	public OrderedClassLoaderTemplateResolver(Comparator<String> comparator) {
		 this(comparator, true);
	}
	
	/**
	 * constructor with custom comparator and option for cache control
	 * @param comparator
	 * @param enableCaching to switch of caching of found templates
	 * @see OrderedClassLoaderResourceResolver#OrderedClassLoaderResourceResolver(Comparator, boolean)
	 * @since 1.0.4
	 */
	public OrderedClassLoaderTemplateResolver(Comparator<String> comparator, boolean enableCaching) {
		 super();
		 this.resourceResolver = new OrderedClassLoaderResourceResolver(comparator, enableCaching);
	     super.setResourceResolver(this.resourceResolver);
	}
	
	/**
	 * clears the cache of found template urls
	 * @since 1.0.4
	 */
	public void clearCache() {
		this.resourceResolver.clearCache();
	}
}
