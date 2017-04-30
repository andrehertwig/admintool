package de.chandre.admintool.core.security.auth;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Simple login attempt service
 * 
 * @author Andre
 * @since 1.1.5
 */
public class LoginAttemptServiceImpl implements LoginAttemptService {
	
	private static final Log LOGGER = LogFactory.getLog(LoginAttemptServiceImpl.class);

	private final int MAX_ATTEMPT;
	
	private boolean useUserName = true;
	private boolean useRemoteAddress = false;
	
	private final Map<String, LoginAttempt> attemptsCache = new ConcurrentHashMap<>();
	
	public LoginAttemptServiceImpl() {
		this(5);
	}
	
	public LoginAttemptServiceImpl(int maxAttempts) {
		this(maxAttempts, true, false);
	}
	
	public LoginAttemptServiceImpl(int maxAttempts, boolean useUserName, boolean useRemoteAddress) {
		super();
		MAX_ATTEMPT = maxAttempts;
		this.useUserName = useUserName;
		this.useRemoteAddress = useRemoteAddress;
		LOGGER.debug("LoginAttemptService initialized with maxAttempts=" + maxAttempts 
				+ ", using usernames=" + useUserName + ", useRemoteAddress=" + useRemoteAddress);
	}

	@Override
	public void invalidate(String userName) {
		attemptsCache.remove(userName);
	}

	@Override
	public void loginFailed(String userName) {
		LoginAttempt attempt = attemptsCache.get(userName);
		if (null == attempt) {
			attempt = new LoginAttempt();
			if(null != attemptsCache.putIfAbsent(userName, attempt)){
				attempt = attemptsCache.get(userName);
			}
		}
		attempt.getActualAttempt().incrementAndGet();
		attempt.setLastAccessNow();
	}

	@Override
	public boolean isBlocked(String userName) {
		LoginAttempt attempt = attemptsCache.get(userName);
		if (null != attempt) {
			return attempt.getActualAttempt().get() >= MAX_ATTEMPT;
		}
		return false;
	}
	
	public LocalDateTime getLastAccess(String userName) {
		LoginAttempt attempt = attemptsCache.get(userName);
		if (null != attempt) {
			return attempt.getLastAccess();
		}
		return null;
	}
	
	private class LoginAttempt {
		
		private LocalDateTime lastAccess;
		private AtomicInteger actualAttempt = new AtomicInteger(0);
		
		public LocalDateTime getLastAccess() {
			return lastAccess;
		}
		public void setLastAccessNow() {
			this.lastAccess = LocalDateTime.now();
		}
		public AtomicInteger getActualAttempt() {
			return actualAttempt;
		}
	}

	public boolean isUseUserName() {
		return useUserName;
	}

	public void setUseUserName(boolean useUserName) {
		this.useUserName = useUserName;
	}

	public boolean isUseRemoteAddress() {
		return useRemoteAddress;
	}

	public void setUseRemoteAddress(boolean useRemoteAddress) {
		this.useRemoteAddress = useRemoteAddress;
	}
	
	public void clearAttempts() {
		this.attemptsCache.clear();
	}
}