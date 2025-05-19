package com.soobin.soobinzilla.filter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import com.soobin.soobinzilla.dto.auth.AuthPayload;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.enums.Role;
import com.soobin.soobinzilla.service.AuthTokenService;
import com.soobin.soobinzilla.service.DepartmentService;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.CookieUtil;
import com.soobin.soobinzilla.util.ObjectUtil;
import com.soobin.soobinzilla.util.SecurityUtil;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class SecurityAccessFilter extends OncePerRequestFilter {
	
	private final AuthTokenService authService;
	
	private final DepartmentService departmentService;
	
	@Override
	protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
			throws ServletException, IOException {
		
		Authentication auth = null;
		String cookieName = request.getRequestURI().endsWith(SecurityUtil.AUTH_REFRESH_URL) ? ConstantUtil.REFRESH_TOKEN_COOKIE : ConstantUtil.ACCESS_TOKEN_COOKIE;
		
		Optional<String> tokenValue = CookieUtil.getCookieValue(request, cookieName);
		
		if(tokenValue.isPresent()) {
			try {
				String token = tokenValue.get();
				// 디비 토큰 검사
				String type = cookieName.equals(ConstantUtil.REFRESH_TOKEN_COOKIE) ? ConstantUtil.REFRESH_TOKEN : ConstantUtil.ACCESS_TOKEN;
				authService.checkToken(token, type);
				// 토큰 속 검사
				AuthPayload payload = authService.checkTokenAndGetPayload(token);
				
				// 매니저 URLS면 MANAGER인지 검사
				Collection<GrantedAuthority> role = getUserAuthByGrantedAuthority(payload.getRole()); 
				if(Boolean.TRUE.equals(isManagerUrls(request.getRequestURI())) && Role.MEMBER.equals(payload.getRole())) {
					Department department = departmentService.getByManagerId(payload.getId());
					if(Boolean.TRUE.equals(ObjectUtil.isEmpty(department))) role = null;
				} 
				
				auth = new UsernamePasswordAuthenticationToken(payload, token, role);				
			} catch (FileTransferException e) {
				auth = null;
			}			
		}
		
		SecurityContextHolder.getContext().setAuthentication(auth);
		filterChain.doFilter(request, response);
	}
	
	public Collection<GrantedAuthority> getUserAuthByGrantedAuthority(Role role) {
		List<GrantedAuthority> authorities = new ArrayList<>();
		authorities.add(new SimpleGrantedAuthority(String.format("ROLE_%s", role)));
		return authorities;
	}
	
	private Boolean isManagerUrls(String requestURI) {
		return SecurityUtil.MANAGER_URLS.stream()
			.anyMatch(requestURI::endsWith);
	}

}
