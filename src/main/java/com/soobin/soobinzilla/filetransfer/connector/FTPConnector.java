package com.soobin.soobinzilla.filetransfer.connector;

import java.io.IOException;
import java.net.ConnectException;
import java.net.SocketTimeoutException;

import org.apache.commons.net.MalformedServerReplyException;
import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPReply;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.ConnectionErrorCode;
import com.soobin.soobinzilla.filetransfer.core.AbstractConnector;
import com.soobin.soobinzilla.filetransfer.factory.FTPClientFactory;
import com.soobin.soobinzilla.model.Connection;
import com.soobin.soobinzilla.model.FtpConfig;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.LogUtil;

public class FTPConnector extends AbstractConnector<FTPClient> {
	
	private FTPClientFactory clientFactory = FTPClientFactory.getInstance();
	private FTPClient client;
	
	public FTPConnector() {
		this.client = clientFactory.getClient();
	}

	@Override
	public void connect(Connection connection) throws FileTransferException {
		super.validConnection(connection);
		super.connection = connection;
		FtpConfig config = (FtpConfig) connection.getConnectionConfig().getProtocolConfig();
		setEncoding(config.getEncoding());
		getConnect(connection.getHost(), connection.getPort());
		LogUtil.connection(connection.getId(), ConstantUtil.SUCCESS_CONNECT);
		setConfig(config);
		getLogin(connection.getUsername(), connection.getPassword());
		LogUtil.connection(connection.getId(), ConstantUtil.SUCCESS_LOGIN);
	}
	
	private void setEncoding(String encoding) {
		this.client.setControlEncoding(encoding);
	}
	
	private void getConnect(String host, Integer port) throws FileTransferException {
		checkPort(port);
		
		try {
			this.client.connect(host, port);
			int replyCode = this.client.getReplyCode();
			if(!FTPReply.isPositiveCompletion(replyCode)) throw new FileTransferException(ConnectionErrorCode.CONNECTION_FAILED);
		} catch (IOException e) {
			if(e instanceof ConnectException) {
				throw new FileTransferException(ConnectionErrorCode.CONNECTION_ERROR);
			} else if(e instanceof MalformedServerReplyException) {
				throw new FileTransferException(ConnectionErrorCode.INVALID_PORT);
			} else if(e instanceof SocketTimeoutException) {
				throw new FileTransferException(ConnectionErrorCode.CONNECTION_TIMEOUT);
			} else {
				throw new FileTransferException(e);
			}
		}
	}
	
	private void setConfig(FtpConfig config) {
		try {
			this.client.setFileType(config.getFileType());
			this.client.setAutodetectUTF8(config.getAutoDetectUtf8());
			this.client.setFileTransferMode(config.getFileTransferMode());
			this.client.setConnectTimeout(config.getConnectTime());
			this.client.setDataTimeout(config.getDataTime());
			if(Boolean.TRUE.equals(config.getPassiveMode())) this.client.enterLocalPassiveMode();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	private void getLogin(String username, String password) throws FileTransferException {		
		try {
			Boolean isLogin = this.client.login(username, password);
			if(Boolean.FALSE.equals(isLogin)) throw new FileTransferException(ConnectionErrorCode.LOGIN_ERROR);
		} catch (IOException e) {
			throw new FileTransferException(e);
		}		
	}

	@Override
	public FTPClient getClient() {
		return this.client;
	}

	@Override
	public FTPClient resetClient() throws FileTransferException {
		this.client = this.clientFactory.resetClient();
		connect(super.connection);
		return this.client;
	}

	@Override
	public void disconnect() throws FileTransferException {		
		try {
			if (this.client.isConnected()) {
				this.client.logout();
				this.client.disconnect();
				LogUtil.connection(super.connection.getId(), ConstantUtil.SUCCESS_DISCONNECT);
			}			
		} catch (IOException e) {
			throw new FileTransferException(ConnectionErrorCode.SERVER_ERROR, e.getMessage());
		}		
	}

	
}
