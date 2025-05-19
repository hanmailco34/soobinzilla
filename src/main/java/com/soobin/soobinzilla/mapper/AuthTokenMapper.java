package com.soobin.soobinzilla.mapper;

import org.springframework.stereotype.Component;

import com.soobin.soobinzilla.model.AuthToken;
import com.soobin.soobinzilla.model.User;

@Component
public class AuthTokenMapper {
	
	public AuthToken toLoginToken(User user, String accessToken, String refreshToken, String ip) {
		return AuthToken.builder()
				.user(user)
				.ipAddress(ip)
				.accessToken(accessToken)
				.refreshToken(refreshToken)
				.build();
	}
}
