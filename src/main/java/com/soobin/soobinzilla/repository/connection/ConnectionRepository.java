package com.soobin.soobinzilla.repository.connection;

import org.springframework.data.jpa.repository.JpaRepository;

import com.soobin.soobinzilla.model.Connection;

public interface ConnectionRepository extends JpaRepository<Connection, Long>, ConnectionRepositoryCustom {
	
}
