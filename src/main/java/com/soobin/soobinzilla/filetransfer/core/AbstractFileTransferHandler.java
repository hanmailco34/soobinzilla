package com.soobin.soobinzilla.filetransfer.core;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.ConnectionErrorCode;
import com.soobin.soobinzilla.filetransfer.task.AbstractTaskStrategy;
import com.soobin.soobinzilla.filetransfer.task.Task;
import com.soobin.soobinzilla.filetransfer.task.TaskVO;
import com.soobin.soobinzilla.filetransfer.task.strategy.DownloadStrategy;
import com.soobin.soobinzilla.filetransfer.task.strategy.UploadStrategy;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;
import com.soobin.soobinzilla.filetransfer.vo.FileTransferAction;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.LogUtil;
import com.soobin.soobinzilla.util.PathUtil;

public abstract class AbstractFileTransferHandler<T> implements IFileTransferHandler<T> {

	FileTransferThread fileTransferThread = FileTransferThread.getInstance();
	
	protected abstract void download(T client, Long connectionId, FileInfoVO file, String localDirectory) throws FileTransferException;
	
	protected abstract void upload(T client, Long connectionId, String localFilePath, String uploadFilePath) throws FileTransferException;
	
	protected abstract void delete(T client, Long connectionId, String deleteFilePath) throws FileTransferException;
	
	protected abstract Boolean forderExists(T client, String path) throws FileTransferException;
	
	protected abstract void forderMake(T client, String path) throws FileTransferException;
	
	protected void handleDownload(T client, Long connectionId, FileInfoVO file, String localDirectory, Boolean isThread) throws FileTransferException {
		if(Boolean.TRUE.equals(isThread)) {
			this.threadHandler(client, connectionId, file, new DownloadStrategy<>(this), localDirectory, null);
		} else {
			this.download(client, connectionId, file, localDirectory);
		}
	}
	
	protected void handleUpload(T client, Long connectionId, FileInfoVO file, String localDirectory, String serverDirectory,
			Boolean isThread) throws FileTransferException {
		if(Boolean.TRUE.equals(isThread)) {
			this.threadHandler(client, connectionId, file, new UploadStrategy<>(this), localDirectory, serverDirectory);
		} else {
			String localFilePath = PathUtil.getLocalFilePath(localDirectory, file.getPath(), file.getName());
			String uploadDirectoryPath = serverDirectory + file.getPath();
			String uploadFilePath = PathUtil.getUploadFilePath(uploadDirectoryPath, file.getName());
			this.makeUploadDirectory(client, uploadDirectoryPath);
			this.upload(client, connectionId, localFilePath, uploadFilePath);
		}		
	}
	
	protected void handleDelete(T client, Long connectionId, FileInfoVO file, String serverDirectory, Boolean isThread) throws FileTransferException {
		String directoryPath = serverDirectory + file.getPath();
		String filePath = PathUtil.getUploadFilePath(directoryPath, file.getName());
		this.delete(client, connectionId, filePath);
	}
	
	protected void makeUploadDirectory(T client, String serverDirectory) throws FileTransferException {
		String[] dirs = serverDirectory.split(ConstantUtil.FILE_SEPARATOR);
		StringBuilder path = new StringBuilder();
		for (String dir : dirs) {
			if (dir.isEmpty()) continue;
			path.append(ConstantUtil.FILE_SEPARATOR + dir);
			
			if(Boolean.FALSE.equals(forderExists(client, path.toString()))) {
				forderMake(client, path.toString());
			}
		}	
	}
	
	protected void threadHandler(T client, Long connectionId, FileInfoVO file, AbstractTaskStrategy<T> taskStrategy, String localDirectory, String serverDirectory) {
		TaskVO<T> taskVO = new TaskVO<>();
		taskVO.setClient(client);
		taskVO.setConnectionId(connectionId);
		taskVO.setLocalDirectory(localDirectory);
		taskVO.setServerDirectory(serverDirectory);
		taskVO.setFileInfoVO(file);
		taskVO.setTaskStrategy(taskStrategy);
		
		Task<T> task = new Task<>(taskVO);
		
		fileTransferThread.add(task);
	}
	
	public void threadFinishHandler() throws FileTransferException {
		try {
			fileTransferThread.waitFileTransferThread();
		} catch (InterruptedException e) {
			fileTransferThread.shutdownNow();
			throw new FileTransferException(ConnectionErrorCode.THREAD_INTERRUPT);			
		}
	}
	
	protected void finishFileTransfer(Long connectionId, String file, FileTransferAction action) {
		String log = null;
		if(FileTransferAction.DOWNLOAD.equals(action)) {
			log = String.format("%s 파일을 서버로 순간이동 시켰습니다. 배송할 준비 완료입니다!", file);
		} else if(FileTransferAction.UPLOAD.equals(action)) {
			log = String.format("%s 파일을 순식간에 업로드 시켰습니다. 안전하게 파일 배달 성공했습니다!", file);
		} else if(FileTransferAction.DELETE.equals(action)) {
			log = String.format("%s 파일은 이제 영영 볼 수 없네요. 그 누구라도 파일을 다시 찾을 수 없을 것입니다!", file);
		}
		
		LogUtil.connection(connectionId, log);
	}
}
