package com.soobin.soobinzilla.filetransfer.handler;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.SftpException;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.FileErrorCode;
import com.soobin.soobinzilla.filetransfer.core.AbstractFileTransferHandler;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;
import com.soobin.soobinzilla.filetransfer.vo.FileTransferAction;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.FileUtil;

public class SFTPFileTransferHandler extends AbstractFileTransferHandler<ChannelSftp> {

	@Override
	public void downloadHandler(ChannelSftp client, Long connectionId, FileInfoVO file, String localDirectory,
			Boolean isThread) throws FileTransferException {
		super.handleDownload(client, connectionId, file, localDirectory, isThread);
	}

	@Override
	public void uploadHandler(ChannelSftp client, Long connectionId, FileInfoVO file, String localDirectory,
			String serverDirectory, Boolean isThread) throws FileTransferException {
		super.handleUpload(client, connectionId, file, localDirectory, serverDirectory, isThread);
	}

	@Override
	public void deleteHandler(ChannelSftp client, Long connectionId, FileInfoVO file, String serverDirectory,
			Boolean isThread) throws FileTransferException {
		super.handleDelete(client, connectionId, file, serverDirectory, isThread);
	}

	@Override
	protected void download(ChannelSftp client, Long connectionId, FileInfoVO file, String localDirectory)
			throws FileTransferException {
		try(
			InputStream is = client.get(file.getPath() + ConstantUtil.FILE_SEPARATOR + file.getName()); ) {
			if(is != null) {
				FileUtil.writeFile(localDirectory + file.getPath(), file.getName(), is);
				super.finishFileTransfer(connectionId, file.getPath() + ConstantUtil.FILE_SEPARATOR + file.getName(), FileTransferAction.DOWNLOAD);
			}
		} catch (IOException e) {
			e.printStackTrace();
			throw new FileTransferException(FileErrorCode.READ_ERROR);
		} catch (SftpException e) {			
			if(ChannelSftp.SSH_FX_PERMISSION_DENIED == e.id) {
				file.setStatus(ConstantUtil.NO_DOWNLOAD_STATUS);
			} else {
				throw new FileTransferException(e);
			}
		}
	}

	@Override
	protected void upload(ChannelSftp client, Long connectionId, String localFilePath, String uploadFilePath)
			throws FileTransferException {
		try (FileInputStream fis = new FileInputStream(localFilePath)) {
			client.put(fis, uploadFilePath);
			super.finishFileTransfer(connectionId, uploadFilePath, FileTransferAction.UPLOAD);
		} catch (FileNotFoundException e) {
			throw new FileTransferException(FileErrorCode.NOT_EXIST_FILE);
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.READ_ERROR);
		} catch (SftpException e) {
			throw new FileTransferException(e);
		}		
	}

	@Override
	protected void delete(ChannelSftp client, Long connectionId, String deleteFilePath) throws FileTransferException {
		try {
			client.rm(deleteFilePath);
			super.finishFileTransfer(connectionId, deleteFilePath, FileTransferAction.DELETE); 
		} catch (SftpException e) {
			if(ChannelSftp.SSH_FX_NO_SUCH_FILE == e.id) {
				throw new FileTransferException(FileErrorCode.NOT_DELETE_FILE);
			} else {
				throw new FileTransferException(e);
			}
		}
	}

	@Override
	protected Boolean forderExists(ChannelSftp client, String path) throws FileTransferException {
		try {
			client.lstat(path);
			return true;
		} catch (SftpException e) {
			if(ChannelSftp.SSH_FX_NO_SUCH_FILE == e.id) {
				return false;
			} else {
				throw new FileTransferException(e);
			}
		}
	}

	@Override
	protected void forderMake(ChannelSftp client, String path) throws FileTransferException {
		try {
			client.mkdir(path);
		} catch (SftpException e) {
			throw new FileTransferException(FileErrorCode.NOT_MAKE_DIRECTORY);
		}
	}

}
