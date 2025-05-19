package com.soobin.soobinzilla.filetransfer.transfer;

import java.util.List;
import java.util.Map;

import com.hierynomus.smbj.SmbConfig;
import com.hierynomus.smbj.share.DiskShare;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.filetransfer.connector.SMBConnector;
import com.soobin.soobinzilla.filetransfer.core.AbstractFileTransferProtocol;
import com.soobin.soobinzilla.filetransfer.handler.SMBFileTransferHandler;
import com.soobin.soobinzilla.filetransfer.reader.SMBDirectoryReader;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;
import com.soobin.soobinzilla.model.Connection;

public class SMBFileTransfer extends AbstractFileTransferProtocol<DiskShare> {
	
	private SMBConnector connector;
	private SMBFileTransferHandler handler = new SMBFileTransferHandler();
	private SMBDirectoryReader reader;
	
	public SMBFileTransfer() {
		this.connector = new SMBConnector();		
	}
	
	public SMBFileTransfer(SmbConfig config) {
		this.connector = new SMBConnector(config);
	}

	@Override
	public void connect(Connection connection) throws FileTransferException {
		connector.connect(connection);
	}

	@Override
	public void loadIndex(String path, Boolean isReadStatus) throws FileTransferException {
		super.setFileIndexManager(path, isReadStatus);
		this.reader = new SMBDirectoryReader(this.connector.getClient(), super.getFileIndexManager());
	}

	@Override
	public void readDirectory(String directory, Boolean isRecursive) throws FileTransferException {
		this.reader.readDirectory(directory, isRecursive);
	}
	
	@Override
	public Map<String, List<FileInfoVO>> readPermissionDirectory(String serverDirectory, Long connectionId, String permissionPath, Boolean isRecursive)
			throws FileTransferException {
		return super.getPermissionDirectory(this.reader, serverDirectory, connectionId, permissionPath, isRecursive);
	}

	@Override
	public void writeIndexFile(String localDirectory, String resetWriteMode) throws FileTransferException {
		super.writeIndexFileList(localDirectory, resetWriteMode);
	}

	@Override
	public Map<String, Long> countStatus() {
		return super.getStatusCount();
	}

	@Override
	public void deleteLocalFile(String path, String mode) throws FileTransferException {
		super.deleteLocalFileList(path, mode);
	}

	@Override
	public void download(Boolean isThread, String downloadPath) throws FileTransferException {
		super.download(this.connector, this.handler, isThread, downloadPath);
	}

	@Override
	public void upload(Boolean isThread, String localDirectory, String uploadPath) throws FileTransferException {
		super.upload(this.connector, this.handler, isThread, localDirectory, uploadPath);
	}

	@Override
	public void delete(Boolean isThread, String deletePath) throws FileTransferException {
		super.delete(this.connector, this.handler, isThread, deletePath);
	}

	@Override
	public void deleteWithSuffix(Boolean isThread, String deletePath, String suffix) throws FileTransferException {
		super.delete(this.connector, this.handler, this.reader, isThread, deletePath, suffix);
	}

	@Override
	public void disconnect() throws FileTransferException {
		this.connector.disconnect();
	}
}
