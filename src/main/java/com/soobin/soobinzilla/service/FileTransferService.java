package com.soobin.soobinzilla.service;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.filetransfer.core.FileTransferManager;
import com.soobin.soobinzilla.filetransfer.core.IFileTransferProtocol;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;
import com.soobin.soobinzilla.model.Connection;
import com.soobin.soobinzilla.model.ConnectionConfig;
import com.soobin.soobinzilla.util.ConstantUtil;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class FileTransferService {
	
	@Value("${app.compare.suffix}")
	private String suffix;
	
	public Map<String, List<FileInfoVO>> readPermissionDirectory(Connection connection, Long connectionId, String path) throws FileTransferException {
		ConnectionConfig connectionConfig = connection.getConnectionConfig();
		IFileTransferProtocol protocol = FileTransferManager.getProtocol(connectionConfig.getProtocol());	
		protocol.connect(connection);
		protocol.loadIndex(connectionConfig.getLocalDirectory(), false);
		return protocol.readPermissionDirectory(connectionConfig.getServerDirectory(), connectionId, path, true);
	}

	public Map<String, Long> download(Connection connection) throws FileTransferException {
		ConnectionConfig connectionConfig = connection.getConnectionConfig();
		IFileTransferProtocol protocol = FileTransferManager.getProtocol(connectionConfig.getProtocol());	
		protocol.connect(connection);
		protocol.loadIndex(connectionConfig.getLocalDirectory(), false);
		protocol.readDirectory(connectionConfig.getServerDirectory(), connectionConfig.getIsRecursive());
		protocol.download(connectionConfig.getIsThread(), connectionConfig.getLocalDirectory());
		protocol.writeIndexFile(connectionConfig.getLocalDirectory(), ConstantUtil.MERGE_WRITE_MODE);
		protocol.disconnect();
		return protocol.countStatus(); 
	}
	
	public void upload(Connection connection) throws FileTransferException {
		ConnectionConfig connectionConfig = connection.getConnectionConfig();
		IFileTransferProtocol protocol = FileTransferManager.getProtocol(connectionConfig.getProtocol());
		protocol.connect(connection);
		protocol.loadIndex(connectionConfig.getLocalDirectory(), true);
		protocol.upload(connectionConfig.getIsThread(), connectionConfig.getLocalDirectory(), connectionConfig.getServerDirectory());
		protocol.disconnect();
	}
	
	public void delete(Connection connection) throws FileTransferException {
		ConnectionConfig connectionConfig = connection.getConnectionConfig();
		IFileTransferProtocol protocol = FileTransferManager.getProtocol(connectionConfig.getProtocol());
		protocol.connect(connection);
		protocol.loadIndex(connectionConfig.getLocalDirectory(), true);
		protocol.delete(connectionConfig.getIsThread(), connectionConfig.getServerDirectory());
		protocol.deleteLocalFile(connectionConfig.getLocalDirectory(), ConstantUtil.DELETE_ALL_MODE);
		protocol.disconnect();
	}
	
	public void deleteWithSuffix(Connection connection) throws FileTransferException {
		ConnectionConfig connectionConfig = connection.getConnectionConfig();
		IFileTransferProtocol protocol = FileTransferManager.getProtocol(connectionConfig.getProtocol());
		protocol.connect(connection);
		protocol.loadIndex(connectionConfig.getLocalDirectory(), true);
		protocol.deleteWithSuffix(connectionConfig.getIsThread(), connectionConfig.getServerDirectory(), suffix);
		protocol.deleteLocalFile(connectionConfig.getLocalDirectory(), ConstantUtil.DELETE_ALL_MODE);
		protocol.disconnect();
	}
	
	public void resetIndexFile(Connection connection) throws FileTransferException {
		ConnectionConfig connectionConfig = connection.getConnectionConfig();
		IFileTransferProtocol protocol = FileTransferManager.getProtocol(connectionConfig.getProtocol());
		protocol.loadIndex(connectionConfig.getLocalDirectory(), true);
		protocol.writeIndexFile(connectionConfig.getLocalDirectory(), ConstantUtil.RESET_WRITE_MODE);
	}
}
