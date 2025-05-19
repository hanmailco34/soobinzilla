package com.soobin.soobinzilla.util;

import java.util.Arrays;
import java.util.Optional;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.stereotype.Component;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@Component
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CookieUtil {
    
    private String name;
    private String value;
    
    @Builder.Default
    private String path = "/";
    
    @Builder.Default
    private Integer age = -1;
    
    @Builder.Default
    private Boolean httpOnly = true;
    
    @Builder.Default
    private Boolean secure = true;
    
    public static CookieUtil sessionCookie(String name, String value) {
    	return ageCookie(name, value, -1);
    }
    
    public static CookieUtil ageCookie(String name, String value, Integer age) {
    	return CookieUtil.builder()
    			.name(name)
    			.value(value)
    			.age(age)
    			.build();
    }
    
    public static Optional<String> getCookieValue(HttpServletRequest request, String name) {
        return Optional.ofNullable(request.getCookies())
        		.flatMap(cookies -> Arrays.stream(cookies)
                .filter(cookie -> cookie.getName().equals(name))
                .map(Cookie::getValue)
                .findFirst());
    }
	
	public void addCookie(HttpServletResponse response) {
		Cookie cookie = createCookie();
		response.addCookie(cookie);
	}
	
	public Optional<String> getCookieValue(HttpServletRequest request) {
        return Optional.ofNullable(request.getCookies())
        		.flatMap(cookies -> Arrays.stream(cookies)
                .filter(cookie -> cookie.getName().equals(this.name))
                .map(Cookie::getValue)
                .findFirst());
    }
	
	public void deleteCookie(HttpServletResponse response) {
        Cookie cookie = createCookie();
        cookie.setMaxAge(0);
        response.addCookie(cookie);
    }
	
	private Cookie createCookie() {
		Cookie cookie = new Cookie(this.name, this.value);
		cookie.setHttpOnly(this.httpOnly);
		cookie.setSecure(this.secure);
		cookie.setPath(this.path);
		cookie.setMaxAge(this.age);
		return cookie;
	}
}
