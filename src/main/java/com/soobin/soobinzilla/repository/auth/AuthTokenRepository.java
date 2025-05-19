package com.soobin.soobinzilla.repository.auth;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import com.soobin.soobinzilla.model.AuthToken;

public interface AuthTokenRepository extends JpaRepository<AuthToken, Long>, AuthTokenRepositoryCustom {
	Optional<AuthToken> findByRefreshTokenAndIsActive(String refreshToken, Boolean isActive);
	Optional<AuthToken> findByAccessTokenAndIsActive(String accessToken, Boolean isActive);
}
