package com.soobin.soobinzilla.config;

import java.util.List;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

import com.soobin.soobinzilla.exception.handler.SecurityAccessDeniedHandler;
import com.soobin.soobinzilla.exception.handler.SecurityAuthenticationHandler;
import com.soobin.soobinzilla.filter.SecurityAccessFilter;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.SecurityUtil;

import lombok.RequiredArgsConstructor;

@Configuration
@EnableWebSecurity
@RequiredArgsConstructor
public class SecurityConfig {
	
	private final SecurityAccessFilter accessFilter;
	private final SecurityAccessDeniedHandler accessDeniedHandler;
	private final SecurityAuthenticationHandler authenticationHandler;
	
	@Value("${app.api.prefix}") 
	private String apiPrefix;
	
	@Bean
	SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
		http
			.csrf(csrf -> csrf.disable())
			.cors(cors -> cors.disable())
			.formLogin(form -> form.disable())
			.authorizeHttpRequests(requests -> requests
					.antMatchers("/swagger-resources/**", "/swagger-ui/**", "/v2/api-docs", "/v3/api-docs").permitAll()
					.antMatchers(getPrefixedUrls(apiPrefix,SecurityUtil.PERMIT_ALL_URLS)).permitAll()
					.antMatchers(getPrefixedUrls(apiPrefix,SecurityUtil.ROLE_ADMIN_URLS)).hasAnyRole(ConstantUtil.ADMIN)
		        	.antMatchers(getPrefixedUrls(apiPrefix,SecurityUtil.ROLE_ALL_URLS)).hasAnyRole(ConstantUtil.ADMIN, ConstantUtil.MEMBER)
		        	.anyRequest().authenticated()
			)
			.exceptionHandling(exceptions -> exceptions
	        	.accessDeniedHandler(accessDeniedHandler)
	        	.authenticationEntryPoint(authenticationHandler)
	    	)
			.addFilterBefore(accessFilter, UsernamePasswordAuthenticationFilter.class);
		
		return http.build();
	}
	
	private String[] getPrefixedUrls(String prefix, List<String> urls) { 
		return urls.stream()
				.map(url -> prefix + url)
				.toArray(String[]::new);
	}
}