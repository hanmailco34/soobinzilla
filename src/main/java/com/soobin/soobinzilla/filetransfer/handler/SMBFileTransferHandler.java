package com.soobin.soobinzilla.filetransfer.handler;

import java.io.IOException;
import java.io.InputStream;
import java.util.EnumSet;

import com.hierynomus.msdtyp.AccessMask;
import com.hierynomus.mserref.NtStatus;
import com.hierynomus.mssmb2.SMB2CreateDisposition;
import com.hierynomus.mssmb2.SMB2ShareAccess;
import com.hierynomus.mssmb2.SMBApiException;
import com.hierynomus.smbj.share.DiskShare;
import com.hierynomus.smbj.share.File;
import com.hierynomus.smbj.utils.SmbFiles;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.FileErrorCode;
import com.soobin.soobinzilla.filetransfer.core.AbstractFileTransferHandler;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;
import com.soobin.soobinzilla.filetransfer.vo.FileTransferAction;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.FileUtil;

public class SMBFileTransferHandler extends AbstractFileTransferHandler<DiskShare> {

	@Override
	public void downloadHandler(DiskShare client, Long connectionId, FileInfoVO file, String localDirectory,
			Boolean isThread) throws FileTransferException {
		super.handleDownload(client, connectionId, file, localDirectory, isThread);
	}

	@Override
	public void uploadHandler(DiskShare client, Long connectionId, FileInfoVO file, String localDirectory,
			String serverDirectory, Boolean isThread) throws FileTransferException {
		super.handleUpload(client, connectionId, file, localDirectory, serverDirectory, isThread);
	}

	@Override
	public void deleteHandler(DiskShare client, Long connectionId, FileInfoVO file, String serverDirectory,
			Boolean isThread) throws FileTransferException {
		super.handleDelete(client, connectionId, file, serverDirectory, isThread);
	}

	@Override
	protected void download(DiskShare client, Long connectionId, FileInfoVO file, String localDirectory)
			throws FileTransferException {
		try {
			File smbFile = client.openFile(file.getPath() + ConstantUtil.FILE_SEPARATOR + file.getName(), EnumSet.of(AccessMask.GENERIC_READ), null, SMB2ShareAccess.ALL, SMB2CreateDisposition.FILE_OPEN, null);
			try(
				InputStream is = smbFile.getInputStream();
			) {
				FileUtil.writeFile(localDirectory + file.getPath(), file.getName(), is);
				super.finishFileTransfer(connectionId, file.getPath() + ConstantUtil.FILE_SEPARATOR + file.getName(), FileTransferAction.DOWNLOAD);
			} catch (IOException e) {
				throw new FileTransferException(FileErrorCode.READ_ERROR);
			}
		} catch(SMBApiException e) {
			if(NtStatus.STATUS_ACCESS_DENIED.equals(e.getStatus())) {
				file.setStatus(ConstantUtil.NO_DOWNLOAD_STATUS);
			}
		}	
	}

	@Override
	protected void upload(DiskShare client, Long connectionId, String localFilePath, String uploadFilePath)
			throws FileTransferException {
		try {
			java.io.File file = new java.io.File(localFilePath);
			SmbFiles.copy(file, client, uploadFilePath, true);
			super.finishFileTransfer(connectionId, uploadFilePath, FileTransferAction.UPLOAD);
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.UPLOAD_ERROR);
		}
	}

	@Override
	protected void delete(DiskShare client, Long connectionId, String deleteFilePath) throws FileTransferException {
		if(client.fileExists(deleteFilePath)) {
			client.rm(deleteFilePath);
			super.finishFileTransfer(connectionId, deleteFilePath, FileTransferAction.DELETE);
		} else {
			throw new FileTransferException(FileErrorCode.NOT_DELETE_FILE);
		}		
	}

	@Override
	protected Boolean forderExists(DiskShare client, String path) throws FileTransferException {
		return client.folderExists(path);
	}

	@Override
	protected void forderMake(DiskShare client, String path) throws FileTransferException {
		client.mkdir(path);
	}
	

}
