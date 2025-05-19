package com.soobin.soobinzilla.filetransfer.connector;

import java.io.IOException;
import java.net.ConnectException;

import com.hierynomus.mssmb2.SMBApiException;
import com.hierynomus.smbj.SMBClient;
import com.hierynomus.smbj.SmbConfig;
import com.hierynomus.smbj.auth.AuthenticationContext;
import com.hierynomus.smbj.connection.Connection;
import com.hierynomus.smbj.session.Session;
import com.hierynomus.smbj.share.DiskShare;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.ConnectionErrorCode;
import com.soobin.soobinzilla.filetransfer.core.AbstractConnector;
import com.soobin.soobinzilla.filetransfer.factory.SMBClientFactory;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.LogUtil;

public class SMBConnector extends AbstractConnector<DiskShare> {
	
	private SMBClientFactory clientFactory = SMBClientFactory.getInstance();
	private SMBClient client;
	private Connection connection;
	private Session session;
	private DiskShare diskShare;
	
	public SMBConnector() {
		this.client = clientFactory.getClient();
	}
	
	public SMBConnector(SmbConfig config) {
		this.clientFactory = SMBClientFactory.getInstance(config);
		this.client = clientFactory.getClient();
	}

	@Override
	public void connect(com.soobin.soobinzilla.model.Connection connection) throws FileTransferException {
		super.validConnection(connection);
		super.connection = connection;
		com.soobin.soobinzilla.model.SmbConfig config = (com.soobin.soobinzilla.model.SmbConfig) connection.getConnectionConfig().getProtocolConfig();
		getConnect(connection.getHost(), connection.getPort());
		getLogin(connection.getUsername(), connection.getPassword(), config.getDomain());
		LogUtil.connection(connection.getId(), ConstantUtil.SUCCESS_CONNECT);
		connectShare(config.getSection());
	}
	
	private void getConnect(String host, Integer port) throws FileTransferException {		
		super.checkPort(port);
		
		try {			
			this.connection = this.client.connect(host, port);
			
		} catch (IOException e) {
			if(e instanceof ConnectException) {
				throw new FileTransferException(ConnectionErrorCode.CONNECTION_ERROR);
			} else {
				throw new FileTransferException(e);
			}
		}		
	}
	
	private void getLogin(String username, String password, String domain) throws FileTransferException {
		AuthenticationContext authContext = new AuthenticationContext(username, password.toCharArray(), domain);
		try {
			this.session = this.connection.authenticate(authContext);	
		} catch(SMBApiException e) {
			throw new FileTransferException(ConnectionErrorCode.SMB_AUTHENTICATION_ERROR);
		}
	}
	
	private void connectShare(String shareName) throws FileTransferException {
		try {
			this.diskShare = (DiskShare) this.session.connectShare(shareName);
		} catch(SMBApiException e) {
			e.printStackTrace();
			throw new FileTransferException(ConnectionErrorCode.SMB_NOT_EXIST_SHARE);
		}
	}

	@Override
	public DiskShare getClient() {
		return this.diskShare;
	}

	@Override
	public DiskShare resetClient() throws FileTransferException {
		this.client = this.clientFactory.resetClient();
		connect(super.connection);
		return this.diskShare;
	}

	@Override
	public void disconnect() throws FileTransferException {
		try {
			this.diskShare.close();
			this.session.close();
			this.connection.close();
			this.client.close();
			LogUtil.connection(super.connection.getId(), ConstantUtil.SUCCESS_DISCONNECT);
		} catch (IOException e) {
			throw new FileTransferException(ConnectionErrorCode.SERVER_ERROR, e.getMessage());
		}
	}

}
