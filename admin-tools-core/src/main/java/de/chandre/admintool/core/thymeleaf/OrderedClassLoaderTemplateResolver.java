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

	public OrderedClassLoaderTemplateResolver() {
		 this(new TemplateUrlComparator());
	}

	/**
	 * constructor with custom comparator
	 * @param comparator
	 */
	public OrderedClassLoaderTemplateResolver(Comparator<String> comparator) {
		 super();
	     super.setResourceResolver(new OrderedClassLoaderResourceResolver(comparator));
	}

}
