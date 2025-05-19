package com.soobin.soobinzilla.filetransfer.connector;

import java.util.Properties;

import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.JSchUnknownHostKeyException;
import com.jcraft.jsch.Session;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.ConnectionErrorCode;
import com.soobin.soobinzilla.filetransfer.core.AbstractConnector;
import com.soobin.soobinzilla.filetransfer.factory.SFTPClientFactory;
import com.soobin.soobinzilla.model.Connection;
import com.soobin.soobinzilla.model.SftpConfig;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.LogUtil;

public class SFTPConnector extends AbstractConnector<ChannelSftp> {
	
	private SFTPClientFactory clientFactory = SFTPClientFactory.getInstance();
	private JSch client;
	private Session session;
	private ChannelSftp channelSftp;
	
	public SFTPConnector() {
		this.client = clientFactory.getClient();
	}

	@Override
	public void connect(Connection connection) throws FileTransferException {
		try {
			super.validConnection(connection);
			super.connection = connection;
			SftpConfig config = (SftpConfig) connection.getConnectionConfig().getProtocolConfig();
			getConnect(connection.getUsername(), connection.getHost(), connection.getPort());
			LogUtil.connection(connection.getId(), ConstantUtil.SUCCESS_CONNECT);
			getLogin(connection.getPassword());
			setConfig(config);			
			openSFTPChannel();
			LogUtil.connection(connection.getId(), ConstantUtil.SUCCESS_LOGIN);
		}  catch (JSchException e) {
			if(e instanceof JSchUnknownHostKeyException) {
				throw new FileTransferException(ConnectionErrorCode.SFTP_UNKOWN_HOST_KEY);				
			} else {
				if(e.getMessage().contains("HostKey")) throw new FileTransferException(ConnectionErrorCode.REJECT_HOSTKEY);
				else if(e.getMessage().contains("session is down")) throw new FileTransferException(ConnectionErrorCode.SESSION_DOWN);
				else throw new FileTransferException(ConnectionErrorCode.LOGIN_ERROR);
			}
		}
	}
	
	private void getConnect(String username, String host, Integer port) throws FileTransferException {
		super.checkPort(port);
		
		try {
			this.session = this.client.getSession(username, host, port);
		} catch (JSchException e) {
			throw new FileTransferException(e);
		}
	}
	
	private void getLogin(String password) {		
		this.session.setPassword(password);
	}
	
	private void setConfig(SftpConfig config) {
		Properties properties = new Properties();
		properties.put("StrictHostKeyChecking", (Boolean.TRUE.equals(config.getHostKeyEnabled())) ? "yes" : "no");
        this.session.setConfig(properties);
	}
	
	private void openSFTPChannel() throws JSchException {
		this.session.connect();
		this.channelSftp = (ChannelSftp) this.session.openChannel("sftp");
        this.channelSftp.connect();
	}

	@Override
	public ChannelSftp getClient() {
		return this.channelSftp;
	}

	@Override
	public ChannelSftp resetClient() throws FileTransferException {
		this.client = this.clientFactory.resetClient();
		connect(super.connection);
		return this.channelSftp;
	}

	@Override
	public void disconnect() throws FileTransferException {
		if (this.channelSftp != null) {
            this.channelSftp.disconnect();
        }
        if (this.session != null) {
            this.session.disconnect();
        }
        LogUtil.connection(super.connection.getId(), ConstantUtil.SUCCESS_DISCONNECT);
	}

}
