package com.soobin.soobinzilla.util;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import com.soobin.soobinzilla.dto.auth.AuthPayload;

@Component
public class AuthUtil {

	private Authentication getAuthentication() {
		return SecurityContextHolder.getContext().getAuthentication();
	}
	
	public AuthPayload getPayload() {
		Authentication authentication = getAuthentication();
		return (AuthPayload) authentication.getPrincipal();
	}
	
	public String getToken() {
		Authentication authentication = getAuthentication();
		return String.valueOf(authentication.getCredentials());
	}
}
