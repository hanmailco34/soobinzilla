package com.soobin.soobinzilla.filetransfer.core;

import java.util.List;
import java.util.Map;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;
import com.soobin.soobinzilla.model.Connection;

public interface IFileTransferProtocol {
	void connect(Connection connection) throws FileTransferException;
	
	void loadIndex(String path, Boolean isReadStatus) throws FileTransferException;
	
	void readDirectory(String directory, Boolean isRecursive) throws FileTransferException;
	
	Map<String, List<FileInfoVO>> readPermissionDirectory(String serverDirectory, Long connectionId, String permissionPath, Boolean isRecursive) throws FileTransferException;
	
	void writeIndexFile(String localDirectory, String resetWriteMode) throws FileTransferException;
	
	Map<String, Long> countStatus();
	
	void deleteLocalFile(String path, String mode) throws FileTransferException;
	
	void download(Boolean isThread, String downloadPath) throws FileTransferException;	
	
	void upload(Boolean isThread, String localDirectory, String uploadPath) throws FileTransferException;
	
	void delete(Boolean isThread, String deletePath) throws FileTransferException;
	
	void deleteWithSuffix(Boolean isThread, String deletePath, String suffix) throws FileTransferException;
	
	void disconnect() throws FileTransferException;
}
