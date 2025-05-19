package com.soobin.soobinzilla.filetransfer.handler;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import org.apache.commons.net.ftp.FTPClient;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.ConnectionErrorCode;
import com.soobin.soobinzilla.exception.code.FileErrorCode;
import com.soobin.soobinzilla.filetransfer.core.AbstractFileTransferHandler;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;
import com.soobin.soobinzilla.filetransfer.vo.FileTransferAction;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.FileUtil;

public class FTPFileTransferHandler extends AbstractFileTransferHandler<FTPClient> {

	@Override
	public void downloadHandler(FTPClient client, Long connectionId, FileInfoVO file, String localDirectory, Boolean isThread) throws FileTransferException {
		super.handleDownload(client, connectionId, file, localDirectory, isThread);
	}

	@Override
	public void uploadHandler(FTPClient client, Long connectionId, FileInfoVO file, String localDirectory, String serverDirectory,
			Boolean isThread) throws FileTransferException {
		super.handleUpload(client, connectionId, file, localDirectory, serverDirectory, isThread);
	}

	@Override
	public void deleteHandler(FTPClient client, Long connectionId, FileInfoVO file, String serverDirectory, Boolean isThread) throws FileTransferException {
		super.handleDelete(client, connectionId, file, serverDirectory, isThread);
	}

	@Override
	protected void download(FTPClient client, Long connectionId, FileInfoVO file, String localDirectory) throws FileTransferException {
		try(
			InputStream is = client.retrieveFileStream(file.getPath() + ConstantUtil.FILE_SEPARATOR + file.getName()); ) {
			if(is != null) {
				FileUtil.writeFile(localDirectory + file.getPath(), file.getName(), is);
				
				boolean success = client.completePendingCommand();
				if(!success) throw new FileTransferException(ConnectionErrorCode.NOT_COMPLETE_PENDING);
				super.finishFileTransfer(connectionId, file.getPath() + ConstantUtil.FILE_SEPARATOR + file.getName(), FileTransferAction.DOWNLOAD);
			} else {
				file.setStatus(ConstantUtil.NO_DOWNLOAD_STATUS);
			}
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.READ_ERROR);
		}	
	}

	@Override
	protected void upload(FTPClient client, Long connectionId, String localFilePath, String uploadFilePath) throws FileTransferException {
		try (FileInputStream fis = new FileInputStream(localFilePath)) {			
			client.storeFile(uploadFilePath, fis);
			chmodFile(client, 644, uploadFilePath);
			super.finishFileTransfer(connectionId, uploadFilePath, FileTransferAction.UPLOAD);
		} catch (FileNotFoundException e) {
			throw new FileTransferException(FileErrorCode.NOT_EXIST_FILE, localFilePath);
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.READ_ERROR);
		}
	}

	@Override
	protected void delete(FTPClient client, Long connectionId, String deleteFilePath) throws FileTransferException {
		try {
			boolean result = client.deleteFile(deleteFilePath);
			if(result) super.finishFileTransfer(connectionId, deleteFilePath, FileTransferAction.DELETE); 
			else {
				throw new FileTransferException(FileErrorCode.NOT_DELETE_FILE, deleteFilePath);
			}
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.WRITE_ERROR);
		}
	}

	@Override
	protected Boolean forderExists(FTPClient client, String path) throws FileTransferException {
		try {
			return client.changeWorkingDirectory(path);
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.NOT_CHANGE_DIRECTORY);
		}
	}

	@Override
	protected void forderMake(FTPClient client, String path) throws FileTransferException {
		try {
			client.makeDirectory(path);
			chmodFile(client, 755, path);
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.NOT_MAKE_DIRECTORY);
		}		
	}
	
	private void chmodFile(FTPClient client, int chmod, String path) throws FileTransferException {
		String command = "chmod " + chmod + " " + path; 
		boolean success;
		try {
			success = client.sendSiteCommand(command);
			if(!success) throw new FileTransferException(FileErrorCode.NOT_CHMOD_FILE);
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.WRITE_ERROR);
		}		
	}

}
