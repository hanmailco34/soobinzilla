package com.soobin.soobinzilla.filetransfer.reader;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.hierynomus.msfscc.FileAttributes;
import com.hierynomus.msfscc.fileinformation.FileIdBothDirectoryInformation;
import com.hierynomus.smbj.share.DiskShare;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.FileErrorCode;
import com.soobin.soobinzilla.filetransfer.core.AbstractDirectoryReader;
import com.soobin.soobinzilla.filetransfer.core.FileIndexManager;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;
import com.soobin.soobinzilla.util.CompareUtil;

public class SMBDirectoryReader extends AbstractDirectoryReader<FileIdBothDirectoryInformation> {

	private DiskShare diskShare;
	
	public SMBDirectoryReader(DiskShare diskShare, FileIndexManager fileIndexManager) {
		super(fileIndexManager);
		this.diskShare = diskShare;
	}

	@Override
	protected Boolean isDirectory(FileIdBothDirectoryInformation file) {
		return isValidFile(file) && isValidDirectory(file);
	}

	@Override
	protected Boolean isFile(FileIdBothDirectoryInformation file) {
		return isValidFile(file) && !isValidDirectory(file);
	}

	@Override
	protected String getName(FileIdBothDirectoryInformation file) {
		return file.getFileName();
	}

	@Override
	protected FileInfoVO getFileInfo(String directory, FileIdBothDirectoryInformation file) {
		FileInfoVO vo = new FileInfoVO();
		vo.setPath(directory);
		vo.setName(file.getFileName());
		vo.setSize(file.getEndOfFile());
		vo.setTimestamp(file.getLastWriteTime().toEpochMillis());
		return vo;
	}

	@Override
	protected List<FileIdBothDirectoryInformation> listFiles(String directory) throws IOException {
		return diskShare.list(directory);
	}
	
	private Boolean isValidFile(FileIdBothDirectoryInformation file) {
		return !file.getFileName().equals(".") && !file.getFileName().equals("..") && (file.getFileAttributes() & FileAttributes.FILE_ATTRIBUTE_HIDDEN.getValue()) == 0;
	}
	
	private Boolean isValidDirectory(FileIdBothDirectoryInformation file) {
		return (file.getFileAttributes() & FileAttributes.FILE_ATTRIBUTE_DIRECTORY.getValue()) != 0;
	}

	@Override
	protected List<FileInfoVO> getFileListWithSuffix(String directory, String filePath, String fileName, String suffix)
			throws FileTransferException {
		try {
			List<FileIdBothDirectoryInformation> fileList = listFiles(directory);
			List<FileInfoVO> result = new ArrayList<>();
			for(FileIdBothDirectoryInformation file : fileList) {
				if(Boolean.TRUE.equals(CompareUtil.compareFileNameWithSuffix(file.getFileName(), fileName, suffix))) {
					FileInfoVO fileInfo = getFileInfo(filePath, file);
					result.add(fileInfo);
				}
			}
			return result;
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.READ_ERROR);
		}
	}
}
