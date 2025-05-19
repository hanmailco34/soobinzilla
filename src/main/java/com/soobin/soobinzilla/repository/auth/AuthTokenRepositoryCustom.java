package com.soobin.soobinzilla.repository.auth;

public interface AuthTokenRepositoryCustom {
	Long deactivateTokensByUser(Long userId);
}
