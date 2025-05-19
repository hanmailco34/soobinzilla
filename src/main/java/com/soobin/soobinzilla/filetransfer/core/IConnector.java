package com.soobin.soobinzilla.filetransfer.core;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.model.Connection;

public interface IConnector<T> {
	
	public abstract void connect(Connection connection) throws FileTransferException;
	
	public abstract T getClient();
	
	public abstract T resetClient() throws FileTransferException;
	
	public abstract void disconnect() throws FileTransferException;
}
