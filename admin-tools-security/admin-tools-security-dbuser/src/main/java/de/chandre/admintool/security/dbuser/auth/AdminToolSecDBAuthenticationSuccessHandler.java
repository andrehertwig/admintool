package de.chandre.admintool.security.dbuser.auth;

import java.io.IOException;
import java.util.Locale;

import javax.annotation.Resource;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.SavedRequestAwareAuthenticationSuccessHandler;
import org.springframework.security.web.authentication.WebAuthenticationDetails;
import org.springframework.web.servlet.LocaleResolver;

import de.chandre.admintool.core.AdminTool;
import de.chandre.admintool.security.commons.auth.LoginAttemptService;
import de.chandre.admintool.security.dbuser.domain.User;
import de.chandre.admintool.security.dbuser.service.AdminToolSecDBUserDetailsService;

/**
 * 
 * @author André
 * @since 1.2.0
 *
 */
public class AdminToolSecDBAuthenticationSuccessHandler extends SavedRequestAwareAuthenticationSuccessHandler  {

	private LoginAttemptService loginAttemptService;
	
	@Autowired
	private AdminToolSecDBUserDetailsService userDetailsService;
	
	@Resource
	private LocaleResolver localeResolver;
	
	public AdminToolSecDBAuthenticationSuccessHandler() {
		this(AdminTool.ROOTCONTEXT, false, null);
	}
	
	public AdminToolSecDBAuthenticationSuccessHandler(LoginAttemptService loginAttemptService) {
		this(AdminTool.ROOTCONTEXT, false, loginAttemptService);
	}
	
	public AdminToolSecDBAuthenticationSuccessHandler(String targetUrl, boolean alwaysUseDefaultTarget, LoginAttemptService loginAttemptService) {
		super();
		super.setDefaultTargetUrl(targetUrl);
		super.setAlwaysUseDefaultTargetUrl(alwaysUseDefaultTarget);
		this.loginAttemptService = loginAttemptService;
	}
	
	@Override
	public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
			Authentication authentication) throws ServletException, IOException {
		
		userDetailsService.loginSuccess(authentication.getName());
		
		if (null != loginAttemptService) {
			if (loginAttemptService.isUseUserName()) {
				loginAttemptService.invalidate(authentication.getName());
			}
			if (loginAttemptService.isUseRemoteAddress()) {
				WebAuthenticationDetails auth = (WebAuthenticationDetails) authentication.getDetails();
				loginAttemptService.invalidate(auth.getRemoteAddress());
			}
		}
		
		Object principal = authentication.getPrincipal();
        if (User.class.isAssignableFrom(principal.getClass())) {
        	User localeProvider = User.class.cast(principal);
        	Locale locale = localeProvider.getLocaleAsLocale();
            localeResolver.setLocale(request, response, locale != null ? locale : LocaleContextHolder.getLocale());
        }
		super.onAuthenticationSuccess(request, response, authentication);
	}
}
