package com.soobin.soobinzilla.repository.connection;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.soobin.soobinzilla.model.Connection;
import com.soobin.soobinzilla.model.QConnection;
import com.querydsl.jpa.impl.JPAQueryFactory;

@Repository
public class ConnectionRepositoryImpl implements ConnectionRepositoryCustom {
	private final JPAQueryFactory jpaQueryFactory;
	
	public ConnectionRepositoryImpl(JPAQueryFactory jpaQueryFactory) {
		this.jpaQueryFactory = jpaQueryFactory;
	}

	@Override
	public List<Connection> findByIsDownload(Boolean isDownload) {
		QConnection connection = QConnection.connection;
		
		return this.jpaQueryFactory.selectFrom(connection)
				.join(connection.connectionConfig).fetchJoin()
				.where(connection.connectionConfig.isDownload.eq(isDownload))
				.fetch();
	}
}
