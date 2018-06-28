package de.chandre.admintool.security.commons.auth;

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
	
	/**
	 * initializes the LoginAttemptService with maxAttempts=5, useUserName=true and useRemoteAddress=false
	 * 
	 * @see #LoginAttemptServiceImpl(int, boolean, boolean)
	 */
	public LoginAttemptServiceImpl() {
		this(5);
	}
	
	/**
	 * initializes the LoginAttemptService with given maxAttempt, useUserName=true and useRemoteAddress=false
	 * 
	 * @param maxAttempts number of maximum attempts a user can try logging in with wrong password until service marks user as blocked
	 * 
	 * @see #LoginAttemptServiceImpl(int, boolean, boolean)
	 */
	public LoginAttemptServiceImpl(int maxAttempts) {
		this(maxAttempts, true, false);
	}
	
	/**
	 * initializes the LoginAttemptService with given parameters
	 * 
	 * @param maxAttempts number of maximum attempts a user can try logging in with wrong password until service marks user as blocked
	 * @param useUserName used in login failure/success listeners to add user/invalidate username
	 * @param useRemoteAddress used in login failure/success listeners to add user/invalidate remote address (may not work/be used when behind a (reverse)proxy)
	 */
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

	@Override
	public boolean isUseUserName() {
		return useUserName;
	}

	public void setUseUserName(boolean useUserName) {
		this.useUserName = useUserName;
	}

	@Override
	public boolean isUseRemoteAddress() {
		return useRemoteAddress;
	}

	public void setUseRemoteAddress(boolean useRemoteAddress) {
		this.useRemoteAddress = useRemoteAddress;
	}
	
	@Override
	public void clearAttempts() {
		this.attemptsCache.clear();
	}

	@Override
	public int getMaximumAttempts() {
		return MAX_ATTEMPT;
	}
	
}