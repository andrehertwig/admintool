package de.chandre.admintool.core.thymeleaf;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.TreeSet;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.core.io.ResourceLoader;
import org.springframework.util.StringUtils;
import org.thymeleaf.TemplateProcessingParameters;
import org.thymeleaf.resourceresolver.ClassLoaderResourceResolver;
import org.thymeleaf.resourceresolver.IResourceResolver;
import org.thymeleaf.util.ClassLoaderUtils;
import org.thymeleaf.util.Validate;

/**
 * Custom URL resource resolver used by template resolver using a comparator optionally 
 * @author Andre
 * @since 1.0.1
 */
public class OrderedClassLoaderResourceResolver implements IResourceResolver {
	
	private static final Log LOGGER = LogFactory.getLog(OrderedClassLoaderResourceResolver.class);

	public static final String NAME = "ADMINTOOL-CLASSLOADER";
	
	private Comparator<String> comparator;
	
	public OrderedClassLoaderResourceResolver() {
		super();
	}
	
	public OrderedClassLoaderResourceResolver(Comparator<String> templateUrlComparator) {
		super();
		this.comparator = templateUrlComparator;
	}
	 
	@Override
	public String getName() {
		return NAME;
	}

	@Override
	public InputStream getResourceAsStream(TemplateProcessingParameters templateProcessingParameters,
			String resourceName) {
		Validate.notNull(resourceName, "Resource name cannot be null");
		LOGGER.trace("============================================================");
		LOGGER.trace("try loading template resource: " + resourceName);
		
		ClassLoader loader = ClassLoaderUtils.getClassLoader(ClassLoaderResourceResolver.class);
		
		String pathToUse = resourceName;
		if(resourceName.startsWith(ResourceLoader.CLASSPATH_URL_PREFIX)) {
			pathToUse = pathToUse.substring(ResourceLoader.CLASSPATH_URL_PREFIX.length());
		}
		pathToUse = StringUtils.cleanPath(pathToUse);
		if (pathToUse.startsWith("/")) {
			pathToUse = pathToUse.substring(1);
		}
		
		String urlToUse = null;
		try {
			
			Enumeration<URL> resources = loader.getResources(pathToUse);
			TreeSet<String> urls = null != this.comparator ? new TreeSet<>(this.comparator) : new TreeSet<>();
			while (resources.hasMoreElements()) {
				String url = ((URL) resources.nextElement()).toString();
				LOGGER.trace("found: " + url.toString());
				urls.add(url);
			}
			
			LOGGER.trace("resources count: " + (!urls.isEmpty() ? urls.size() : 0));
			
			if (!urls.isEmpty()) {
				urlToUse = urls.first();
				LOGGER.trace("using remplate resource: " + urlToUse);
				return getResourceAsStream(urlToUse);
			}
			
		} catch (IOException e) {
			LOGGER.error(e.getMessage(), e);
		}
		return null;
	}
	
	
	public InputStream getResourceAsStream(String paramString) throws MalformedURLException {
		URL localURL = new URL(paramString);
		try {
			if (null != localURL) {
				URLConnection localURLConnection = localURL.openConnection();
				return localURLConnection.getInputStream();
			}
		} catch (IOException localIOException) {
		}
		return null;
	}

}
