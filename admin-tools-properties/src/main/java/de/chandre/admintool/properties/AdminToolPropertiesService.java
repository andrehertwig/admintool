package de.chandre.admintool.properties;

import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.TreeMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.EnumerablePropertySource;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertySource;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Service;

@Service("adminToolPropertiesService")
public class AdminToolPropertiesService {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolPropertiesService.class);
	
	private Map<String, String> gitProperties = new TreeMap<String, String>();
	private boolean loaded = false;
	
	@Autowired
	private ConfigurableEnvironment env;
	
	@Autowired
	private ApplicationContext applicationContext;
	
	@Autowired
	private AdminToolPropertiesConfig config;
	
	/**
	 * checks if GIT properties are available (loads it if necessary)
	 * @return
	 */
	public synchronized boolean hasGitProperties() {
		if(gitProperties.isEmpty()) {
			loadGitProperties();
		}
		return !gitProperties.isEmpty();
	}
	
	/**
	 * 
	 * @return GIT properties (loads it if necessary)
	 */
	public synchronized Map<String, String> getGitProperties() {
		if(!loaded) {
			loadGitProperties();
		}
		return gitProperties;
	}
	
	private void loadGitProperties() {
		try {
			Resource gitResource = this.applicationContext.getResource(config.getGitPropertiesPath());
			Reader reader = new InputStreamReader(gitResource.getInputStream(), config.getGitPropertiesEncoding());
			Properties p = new Properties();
			p.load(reader);

			for (Entry<Object, Object> entry : p.entrySet()) {
				gitProperties.put(String.valueOf(entry.getKey()), String.valueOf(entry.getValue()));
			}
			this.loaded = true;
		} catch (Exception e) {
			LOGGER.debug(e.getMessage(), e);
		}
	}
	
	/**
	 * returns the spring environment properties
	 * @return
	 */
	public Map<String, String> getEnvProperty() {
		Map<String, String> res = new TreeMap<String, String>();
		MutablePropertySources mps = env.getPropertySources();
        Iterator<PropertySource<?>> iter = mps.iterator();
        while (iter.hasNext()) {
            PropertySource<?> ps = iter.next();
            if (ps instanceof EnumerablePropertySource<?>) {
                for (String propName : ((EnumerablePropertySource<?>) ps).getPropertyNames()) {
                	res.put(propName, env.getProperty(propName));
                }
            }
        }
        return res;
	}
}
