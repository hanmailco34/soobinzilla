package com.soobin.soobinzilla.filetransfer.core;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.FileErrorCode;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;
import com.soobin.soobinzilla.util.ConstantUtil;

public abstract class AbstractDirectoryReader<T> {

	private FileIndexManager fileIndexManager;
	
	protected AbstractDirectoryReader(FileIndexManager fileIndexManager) {
		this.fileIndexManager = fileIndexManager;
	}

	protected abstract Boolean isDirectory(T file);
	
	protected abstract Boolean isFile(T file);
	
	protected abstract String getName(T file);
	
	protected abstract FileInfoVO getFileInfo(String directory, T file);
	
	protected abstract List<T> listFiles(String directory) throws IOException;
	
	protected abstract List<FileInfoVO> getFileListWithSuffix(String directory, String filePath, String fileName, String suffix) throws FileTransferException;
	
	public void readDirectory(String directory, boolean isRecursive) throws FileTransferException {
		try {
			Queue<String> directoriesToVisit = new LinkedList<>();	
			directoriesToVisit.add(directory);
			
			while(!directoriesToVisit.isEmpty()) {
				String currentDirectory = directoriesToVisit.poll();
				
				for(T file : listFiles(currentDirectory)) {
					if(Boolean.TRUE.equals(isFile(file))) {
						FileInfoVO fileInfo = getFileInfo(currentDirectory, file);
						this.fileIndexManager.proccessIndexFiles(currentDirectory, fileInfo);
					} else if(Boolean.TRUE.equals(isDirectory(file) && isRecursive)) {
						String separator = ConstantUtil.FILE_SEPARATOR;
						if(currentDirectory.endsWith(ConstantUtil.FILE_SEPARATOR)) separator = "";
						directoriesToVisit.add(currentDirectory + separator + getName(file));
					}
				}
			}
		} catch(IOException e) {
			e.printStackTrace();
			throw new FileTransferException(FileErrorCode.READ_ERROR);
		}
	}
	
	public Map<String, List<FileInfoVO>> getFileIndex() {
		return fileIndexManager.getPermissionIndexFile();
	}
}
