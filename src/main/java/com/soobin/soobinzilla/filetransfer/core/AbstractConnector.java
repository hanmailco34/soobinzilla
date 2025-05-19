package com.soobin.soobinzilla.filetransfer.core;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.ConnectionErrorCode;
import com.soobin.soobinzilla.model.Connection;
import com.soobin.soobinzilla.util.ObjectUtil;

public abstract class AbstractConnector<T> implements IConnector<T> {
	
	protected Connection connection;
	
	public Connection getConnection() {
		return this.connection;
	}
	
	protected void checkPort(Integer port) throws FileTransferException {
		if(port < 0) throw new FileTransferException(ConnectionErrorCode.PORT_OUT_OF_RANGE);
	}
	
	protected void validConnection(Connection connection) throws FileTransferException {
		if(Boolean.TRUE.equals(ObjectUtil.isEmpty(connection.getHost()))) throw new FileTransferException(ConnectionErrorCode.INVALID_PARAMETER, "항목: HOST");
		if(Boolean.TRUE.equals(ObjectUtil.isEmpty(connection.getPort()))) throw new FileTransferException(ConnectionErrorCode.INVALID_PARAMETER, "항목: PORT");
		if(Boolean.TRUE.equals(ObjectUtil.isEmpty(connection.getUsername()))) throw new FileTransferException(ConnectionErrorCode.INVALID_PARAMETER, "항목: USERNAME");
		if(Boolean.TRUE.equals(ObjectUtil.isEmpty(connection.getPassword()))) throw new FileTransferException(ConnectionErrorCode.INVALID_PARAMETER, "항목: PASSWORD");
	}
}
