package de.chandre.admintool.log4j2;

import java.io.IOException;

import javax.servlet.annotation.WebListener;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@WebListener
@Component
public class AdminToolLog4jSessionListener implements HttpSessionListener {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolLog4jSessionListener.class);
	
	public AdminToolLog4jSessionListener() {
		LOGGER.debug("Session listener to remove useless outputstreams has been initialiezed");
	}
	
	@Autowired
	private AdminToolLog4j2Util log4jUtil;

	@Override
	public void sessionCreated(HttpSessionEvent sessionEvent) {}

	@Override
	public void sessionDestroyed(HttpSessionEvent sessionEvent) {
		String appenderName = (String) sessionEvent.getSession().getAttribute(AdminToolLog4j2Util.SESSION_APPENDER_NAME);
		try {
			log4jUtil.closeOutputStreamAppender(appenderName);
		} catch (IOException e) {
			LOGGER.error(e.getMessage(), e);
		}
	}

}
