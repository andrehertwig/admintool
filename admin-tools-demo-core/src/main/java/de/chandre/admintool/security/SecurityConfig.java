package de.chandre.admintool.security;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.Order;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;

import de.chandre.admintool.core.AdminTool;


@EnableWebSecurity
public class SecurityConfig
{
	@Autowired
	public void configureGlobal(AuthenticationManagerBuilder auth, AdminToolInMemoryUserDetailsService userDetailsService) throws Exception {
		
		AdminToolInMemoryUserDetailsConfigurer<AuthenticationManagerBuilder> imdmc = new AdminToolInMemoryUserDetailsConfigurer<>(userDetailsService);
		imdmc.withUser("operator").password("operator").roles("OPERATOR");
		imdmc.withUser("admin").password("admin").roles("ADMIN", "OPERATOR");
		
		imdmc.configure(auth);
	}
	
	@Configuration
	@Order(1)
	public static class OperatorSecurityConfig extends WebSecurityConfigurerAdapter {
		
		@Override
		protected void configure(HttpSecurity http) throws Exception {
			http
				.headers().frameOptions().sameOrigin().and()
				.requestMatchers().antMatchers("/monitoring", "/jmx/**").and()
				.authorizeRequests()
					.anyRequest().hasAnyRole("OPERATOR", "ADMIN")
				.and().httpBasic();
		}
		
	}
	
	@Configuration
	public static class AdminToolSecurityConfig extends WebSecurityConfigurerAdapter {
		@Override
		public void configure(WebSecurity web) throws Exception {
		    web
		      .ignoring()
		         .antMatchers("/static/**", "/webjars/**"); 
		}
		
		@Override
		protected void configure(HttpSecurity http) throws Exception {
			http
//				.csrf().disable()
				.authorizeRequests()
					.antMatchers(AdminTool.ROOTCONTEXT + "/dbbrowser").hasAnyRole("ADMIN")
					.antMatchers(AdminTool.ROOTCONTEXT + "/dbbrowser/**").hasAnyRole("ADMIN")
					.antMatchers(AdminTool.ROOTCONTEXT).permitAll()
					.antMatchers(AdminTool.ROOTCONTEXT + "/**").permitAll()
				.and()
					.formLogin()
						.loginPage(AdminTool.ROOTCONTEXT + "/login")
						.defaultSuccessUrl(AdminTool.ROOTCONTEXT, false)
						.permitAll()
				.and()
					.logout()
						.logoutUrl(AdminTool.ROOTCONTEXT + "/logout")
						.logoutSuccessUrl(AdminTool.ROOTCONTEXT)
						.invalidateHttpSession(true)
			;
		}
		
	}
}
