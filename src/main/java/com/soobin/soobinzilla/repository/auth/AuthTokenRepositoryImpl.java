package com.soobin.soobinzilla.repository.auth;

import javax.transaction.Transactional;

import org.springframework.stereotype.Repository;

import com.soobin.soobinzilla.model.QAuthToken;
import com.querydsl.jpa.impl.JPAQueryFactory;

@Repository
public class AuthTokenRepositoryImpl implements AuthTokenRepositoryCustom {
	
	private final JPAQueryFactory queryFactory;
	
	public AuthTokenRepositoryImpl(JPAQueryFactory queryFactory) {
        this.queryFactory = queryFactory;
	}

	@Override
	@Transactional
	public Long deactivateTokensByUser(Long userId) {
		QAuthToken authToken = QAuthToken.authToken;
		
		return this.queryFactory.update(authToken)
				.set(authToken.isActive, false)
				.where(authToken.user.id.eq(userId))
				.execute();
	}
}
