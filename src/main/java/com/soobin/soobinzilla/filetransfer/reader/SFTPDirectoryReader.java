package com.soobin.soobinzilla.filetransfer.reader;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.ChannelSftp.LsEntry;
import com.jcraft.jsch.SftpException;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.FileErrorCode;
import com.soobin.soobinzilla.filetransfer.core.AbstractDirectoryReader;
import com.soobin.soobinzilla.filetransfer.core.FileIndexManager;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;
import com.soobin.soobinzilla.util.CompareUtil;

public class SFTPDirectoryReader extends AbstractDirectoryReader<LsEntry> {
	
	private ChannelSftp channelSftp;
	
	public SFTPDirectoryReader(ChannelSftp channelSftp, FileIndexManager fileIndexManager) {
		super(fileIndexManager);
		this.channelSftp = channelSftp;
	}

	@Override
	protected Boolean isDirectory(LsEntry file) {
		return file.getAttrs().isDir();
	}

	@Override
	protected Boolean isFile(LsEntry file) {
		return !file.getAttrs().isDir();
	}

	@Override
	protected String getName(LsEntry file) {
		return file.getFilename();
	}

	@Override
	protected FileInfoVO getFileInfo(String directory, LsEntry file) {
		FileInfoVO vo = new FileInfoVO();
		vo.setPath(directory);
		vo.setName(file.getFilename());
		vo.setSize(file.getAttrs().getSize());
		vo.setTimestamp(file.getAttrs().getMTime() * 1000L);
		return vo;
	}

	@Override
	protected List<LsEntry> listFiles(String directory) throws IOException {
		try {
			return channelSftp.ls(directory).stream().filter(e -> !e.getFilename().startsWith(".")).collect(Collectors.toList());
		} catch (SftpException e) {
			throw new IOException(e);
		}
	}

	@Override
	protected List<FileInfoVO> getFileListWithSuffix(String directory, String filePath, String fileName, String suffix)
			throws FileTransferException {
		try {
			List<LsEntry> fileList = listFiles(directory);
			List<FileInfoVO> result = new ArrayList<>();
			for(LsEntry file : fileList) {
				if(Boolean.TRUE.equals(CompareUtil.compareFileNameWithSuffix(file.getFilename(), fileName, suffix))) {
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
