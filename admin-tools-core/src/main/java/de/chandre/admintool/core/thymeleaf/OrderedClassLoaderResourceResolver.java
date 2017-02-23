package de.chandre.admintool.core.thymeleaf;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Map;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;

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
	
	private boolean cacheEnabled;
	
	private Map<String, String> foundTemplateCache = new ConcurrentHashMap<>();
	
	/**
	 * no special comparator (TreeSet is used) and found template urls are getting cached
	 */
	public OrderedClassLoaderResourceResolver() {
		this(null, true);
	}
	
	/**
	 * custom comparator and found template urls are getting cached
	 * 
	 * @param templateUrlComparator
	 */
	public OrderedClassLoaderResourceResolver(Comparator<String> templateUrlComparator) {
		this(templateUrlComparator, true);
	}
	
	/**
	 * custom comparator and cached control of found template urls could be determined<br>
	 * deactivating the cache has a minimal performance impact (in range of milliseconds), but of course it depends on
	 * how often a template was overwritten and how the comparator works.<br>
	 * if using the {@link TemplateUrlComparator} there are expensive regular expressions inside, so caching could be helpful 
	 * 
	 * @param templateUrlComparator custom comparator
	 * @param enableCaching enable/disable caching of found templates
	 * @since 1.0.4
	 */
	public OrderedClassLoaderResourceResolver(Comparator<String> templateUrlComparator, boolean enableCaching) {
		super();
		this.comparator = templateUrlComparator;
		this.cacheEnabled = enableCaching;
	}
	 
	@Override
	public String getName() {
		return NAME;
	}
	
	/**
	 * clears the cache of found template urls
	 * @since 1.0.4
	 */
	public void clearCache() {
		this.foundTemplateCache.clear();
	}

	@Override
	public InputStream getResourceAsStream(TemplateProcessingParameters templateProcessingParameters,
			String resourceName) {
		Validate.notNull(resourceName, "Resource name cannot be null");
		
		String urlToUse = null;
		if (cacheEnabled) {
			urlToUse = foundTemplateCache.get(resourceName);
			if (null != urlToUse) {
				try {
					LOGGER.trace("returning remplate resource from cache: " + urlToUse);
					return getResourceAsStream(urlToUse);
				} catch (MalformedURLException e) {
					LOGGER.error(e.getMessage(), e);
				}
				return null;
			}
		}
		
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
				foundTemplateCache.put(resourceName, urlToUse);
				LOGGER.trace("using remplate resource: " + urlToUse);
				return getResourceAsStream(urlToUse);
			}
			
		} catch (IOException e) {
			LOGGER.error(e.getMessage(), e);
		}
		return null;
	}
	
	/**
	 * loads the input stream of an url
	 * @param paramString
	 * @return
	 * @throws MalformedURLException
	 */
	protected InputStream getResourceAsStream(String paramString) throws MalformedURLException {
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
