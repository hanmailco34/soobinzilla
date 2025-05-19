package com.soobin.soobinzilla.filetransfer.reader;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPFile;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.FileErrorCode;
import com.soobin.soobinzilla.filetransfer.core.AbstractDirectoryReader;
import com.soobin.soobinzilla.filetransfer.core.FileIndexManager;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;
import com.soobin.soobinzilla.util.CompareUtil;

public class FTPDirectoryReader extends AbstractDirectoryReader<FTPFile> {
	
	private FTPClient client;

	public FTPDirectoryReader(FTPClient client, FileIndexManager fileIndexManager) {
		super(fileIndexManager);
		this.client = client;
	}

	@Override
	protected Boolean isDirectory(FTPFile file) {
		return file.isDirectory();
	}

	@Override
	protected Boolean isFile(FTPFile file) {
		return file.isFile();
	}

	@Override
	protected String getName(FTPFile file) {
		return file.getName();
	}

	@Override
	protected FileInfoVO getFileInfo(String directory, FTPFile file) {
		FileInfoVO vo = new FileInfoVO();
		vo.setPath(directory);
		vo.setName(file.getName());		
		vo.setSize(file.getSize());
		vo.setTimestamp(file.getTimestamp().getTimeInMillis());
		return vo;
	}

	@Override
	protected List<FTPFile> listFiles(String directory) throws IOException {
		client.changeWorkingDirectory(directory);
		return Arrays.asList(client.listFiles(directory));
	}

	@Override
	protected List<FileInfoVO> getFileListWithSuffix(String directory, String filePath, String fileName, String suffix) throws FileTransferException {
		try {
			List<FTPFile> fileList = listFiles(directory);
			List<FileInfoVO> result = new ArrayList<>();
			for(FTPFile file : fileList) {
				if(Boolean.TRUE.equals(CompareUtil.compareFileNameWithSuffix(file.getName(), fileName, suffix))) {
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
