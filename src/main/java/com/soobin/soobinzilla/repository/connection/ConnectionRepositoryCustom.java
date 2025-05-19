package com.soobin.soobinzilla.repository.connection;

import java.util.List;

import com.soobin.soobinzilla.model.Connection;

public interface ConnectionRepositoryCustom {
	List<Connection> findByIsDownload(Boolean isDownload);
}
