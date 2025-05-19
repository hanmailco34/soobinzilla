package com.soobin.soobinzilla.filetransfer.core;

import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.FileUtil;
import com.soobin.soobinzilla.util.ParseUtil;
import com.soobin.soobinzilla.util.PathUtil;
import com.soobin.soobinzilla.util.ZipUtil;

public abstract class AbstractFileTransferProtocol<T> implements IFileTransferProtocol {
	private FileIndexManager fileIndexManager;
	
	protected void setFileIndexManager(String path, Boolean isReadStatus) throws FileTransferException {
		this.fileIndexManager = new FileIndexManager(path, isReadStatus);
	}
	
	protected FileIndexManager getFileIndexManager() {
		return this.fileIndexManager;
	}
	
	protected void writeIndexFileList(String path, String mode) throws FileTransferException {
		if(Boolean.TRUE.equals(getFileIndexManager().isChangeIndexFile())) {
			List<FileInfoVO> fileList = new ArrayList<>();
			
			if(ConstantUtil.MERGE_WRITE_MODE.equals(mode)) fileList = mergeWriteIndexFile();
			else if(ConstantUtil.RESET_WRITE_MODE.equals(mode)) fileList = resetWriteIndexFile();
			
			FileUtil.writeIndex(path, fileList);
		}
	}
	
	private List<FileInfoVO> mergeWriteIndexFile() throws FileTransferException {
		List<FileInfoVO> newFileList = getFileIndexManager().getChangeIndexFile();
		Map<String, List<FileInfoVO>> indexFileMap = getFileIndexManager().readIndexFile(true);
		List<FileInfoVO> originFileList =  ParseUtil.mapToListFileInfo(indexFileMap);
		return ParseUtil.mergeFileList(originFileList, newFileList);
	}
	
	private List<FileInfoVO> resetWriteIndexFile() {
		return getFileIndexManager().getInitIndexFile();
	}
	
	protected Map<String, Long> getStatusCount() {
		return getFileIndexManager().getStatusCount();
	}
	
	protected void deleteLocalFileList(String path, String mode) throws FileTransferException {
		if(ConstantUtil.DELETE_ALL_MODE.equals(mode)) {
			FileUtil.delete(path);
		} else if(ConstantUtil.DELETE_SYNC_MODE.equals(mode)) {
			List<FileInfoVO> fileList = getFileIndexManager().getDeleteIndexFile();
			for(FileInfoVO fileVO : fileList) {
				String filePath = PathUtil.getLocalFilePath(path, fileVO.getPath(), fileVO.getName());
				FileUtil.delete(filePath);
			}
		}
	}
	
	protected void download(AbstractConnector<T> connector, AbstractFileTransferHandler<T> handler, Boolean isThread, String downloadPath) throws FileTransferException {
		List<FileInfoVO> fileList = getFileIndexManager().getTaskIndexFile();

		for(FileInfoVO file : fileList) {
			T client = Boolean.TRUE.equals(isThread) ? connector.resetClient() : connector.getClient();
			handler.downloadHandler(client, connector.getConnection().getId(), file, downloadPath, isThread);
		}
		
		if(Boolean.TRUE.equals(isThread) && !fileList.isEmpty()) {
			handler.threadFinishHandler();
		}		
	}
	
	protected void upload(AbstractConnector<T> connector, AbstractFileTransferHandler<T> handler, Boolean isThread, String localPath, String uploadPath) throws FileTransferException {
		List<FileInfoVO> fileList = getFileIndexManager().getTaskIndexFile();
		
		for(FileInfoVO file : fileList) {
			T client = Boolean.TRUE.equals(isThread) ? connector.resetClient() : connector.getClient();
			String filePath = PathUtil.convertPath(localPath + file.getPath());
			
			if(Boolean.TRUE.equals(ZipUtil.isZipFile(filePath, file.getName()))) {
				ZipUtil.unzip(filePath, file.getName());
				String zipDirectoryPath = PathUtil.convertPath(filePath + ConstantUtil.FILE_SEPARATOR + ZipUtil.getDirectoryname(file.getName()));
				List<Path> unzipFileList = FileUtil.listFilesInDirectory(zipDirectoryPath);
				for(Path unzipPath : unzipFileList) {
					File unzipFile = unzipPath.toFile();
					String path = PathUtil.getRealativePath(localPath, unzipFile.getParent());
					FileInfoVO unzipFileInfo = ParseUtil.toFileInfo(path, unzipFile);
					handler.uploadHandler(client, connector.getConnection().getId(), unzipFileInfo, localPath, uploadPath, isThread);
				}
			} else {
				handler.uploadHandler(client, connector.getConnection().getId(), file, localPath, uploadPath, isThread);
			}
		}
		
		if(Boolean.TRUE.equals(isThread) && !fileList.isEmpty()) {
			handler.threadFinishHandler();
		}		
	}
	
	protected void delete(AbstractConnector<T> connector, AbstractFileTransferHandler<T> handler, Boolean isThread, String deletePath) throws FileTransferException {
		List<FileInfoVO> fileList = getFileIndexManager().getDeleteIndexFile();
		
		for(FileInfoVO file : fileList) {
			T client = Boolean.TRUE.equals(isThread) ? connector.resetClient() : connector.getClient();
			handler.deleteHandler(client, connector.getConnection().getId(), file, deletePath, isThread);
		}
		
		if(Boolean.TRUE.equals(isThread) && !fileList.isEmpty()) {
			handler.threadFinishHandler();
		}		
	}
	
	protected <R> void delete(AbstractConnector<T> connector, AbstractFileTransferHandler<T> handler, AbstractDirectoryReader<R> reader, Boolean isThread, String deletePath, String suffix) throws FileTransferException {
		List<FileInfoVO> fileList = getFileIndexManager().getDeleteIndexFile();
		
		for(FileInfoVO file : fileList) {
			T client = Boolean.TRUE.equals(isThread) ? connector.resetClient() : connector.getClient();
			String directory = deletePath  + file.getPath();
			if(Boolean.FALSE.equals(ZipUtil.isZipFileByExtension(file.getName()))) {
				List<FileInfoVO> suffixFileList = reader.getFileListWithSuffix(directory, file.getPath(), file.getName(), suffix);
				for(FileInfoVO suffixFile : suffixFileList) {
					handler.deleteHandler(client, connector.getConnection().getId(), suffixFile, deletePath, isThread);
				}
			}
		}
		
		if(Boolean.TRUE.equals(isThread) && !fileList.isEmpty()) {
			handler.threadFinishHandler();
		}		
	}
	
	protected <R> Map<String, List<FileInfoVO>> getPermissionDirectory(AbstractDirectoryReader<R> reader, String serverDirectory, Long connectionId, String permissionPath, Boolean isRecursive) throws FileTransferException {
		String directory = serverDirectory + ConstantUtil.FILE_SEPARATOR + connectionId + permissionPath;
		reader.readDirectory(directory, isRecursive);
		return reader.getFileIndex();
	}
}
