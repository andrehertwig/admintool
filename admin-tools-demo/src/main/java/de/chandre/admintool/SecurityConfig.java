package de.chandre.admintool;

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
	public void configureGlobal(AuthenticationManagerBuilder auth) throws Exception {
		auth
			.inMemoryAuthentication()
				.withUser("user").password("user").roles("USER").and()
				.withUser("operator").password("operator").roles("OPERATOR").and()
				.withUser("admin").password("admin").roles("ADMIN", "USER", "OPERATOR");
	}
	
	@Configuration
	@Order(1)
	public static class OperatorSecurityConfig extends WebSecurityConfigurerAdapter {
		
		@Override
		protected void configure(HttpSecurity http) throws Exception {
			http
				.headers().frameOptions().sameOrigin().and()
//				.antMatcher("/monitoring").antMatcher("/jmx/**")
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
				.csrf().disable()
				.authorizeRequests()
					.antMatchers(AdminTool.ROOTCONTEXT + "/dbbrowser").hasAnyRole("ADMIN")
					.antMatchers(AdminTool.ROOTCONTEXT + "/dbbrowser/**").hasAnyRole("ADMIN")
					.antMatchers(AdminTool.ROOTCONTEXT).permitAll()
					.antMatchers(AdminTool.ROOTCONTEXT + "/**").permitAll()
				.and().formLogin().loginPage(AdminTool.ROOTCONTEXT + "/login").permitAll()
			;
		}
		
	}
}
