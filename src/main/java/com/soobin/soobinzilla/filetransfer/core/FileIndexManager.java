package com.soobin.soobinzilla.filetransfer.core;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.FileErrorCode;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.PathUtil;

public class FileIndexManager {

	private final Map<String, List<FileInfoVO>> indexFiles;
	private String rootPath;
	
	public FileIndexManager(String rootPath, Boolean isReadStatus) throws FileTransferException {
		this.rootPath = rootPath;
		this.indexFiles = readIndexFile(isReadStatus);
	}
	
	public List<FileInfoVO> getTaskIndexFile() {
		return this.indexFiles.values().stream()
				.flatMap(Collection::stream)
				.filter(e -> ConstantUtil.INSERT_STATUS.equals(e.getStatus()) || ConstantUtil.UPDATE_STATUS.equals(e.getStatus()))
				.collect(Collectors.toList());
	}
	
	public List<FileInfoVO> getDeleteIndexFile() {
		return this.indexFiles.values().stream()
				.flatMap(Collection::stream)
				.filter(e -> ConstantUtil.DELETE_STATUS.equals(e.getStatus()))
				.collect(Collectors.toList());
	}
	
	public Boolean isChangeIndexFile() {		
		return this.indexFiles.values().stream()
				.flatMap(Collection::stream)
				.anyMatch(e -> !ConstantUtil.NO_CHANGE_STATUS.equals(e.getStatus()));
	}
	
	public List<FileInfoVO> getChangeIndexFile() {
		return this.indexFiles.values().stream()
				.flatMap(Collection::stream)
				.filter(e -> !ConstantUtil.NO_CHANGE_STATUS.equals(e.getStatus()))
				.collect(Collectors.toList());
	}
	
	public List<FileInfoVO> getInitIndexFile() {
		return this.indexFiles.values().stream()
				.flatMap(Collection::stream)
				.filter(e -> !(ConstantUtil.DELETE_STATUS.equals(e.getStatus()) || ConstantUtil.NO_DOWNLOAD_STATUS.equals(e.getStatus())))
				.map(e -> {
					e.setStatus(ConstantUtil.NO_CHANGE_STATUS);
					return e;
				})
				.collect(Collectors.toList());
	}
	
	public Map<String, Long> getStatusCount() {
		return this.indexFiles.values().stream()
				.flatMap(Collection::stream)
				.collect(Collectors.groupingBy(FileInfoVO::getStatus, Collectors.counting()));
	}
	
	public Map<String, List<FileInfoVO>> getPermissionIndexFile() {
		return this.indexFiles;
	}
	
	public Map<String, List<FileInfoVO>> readIndexFile(Boolean isReadStatus) throws FileTransferException {
		String indexFilePath = getIndexFilePath();
		
		try(Stream<String> lines = Files.lines(Paths.get(indexFilePath))) {
			String [] path = new String[1];
			return lines.collect(HashMap::new,
								(result, s) -> processReadLine(result, s, isReadStatus, path),
								HashMap::putAll);
		} catch (NoSuchFileException e) {
			return new HashMap<>();
		} catch (UncheckedIOException e) {
			throw new FileTransferException(FileErrorCode.ENCODING_ERROR);		
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.READ_ERROR);
		}
	}
	
	private String getIndexFilePath() {
		return PathUtil.convertPath(this.rootPath + ConstantUtil.FILE_SEPARATOR + ConstantUtil.INDEX_FILE_NAME);
	}
	
	private void processReadLine(Map<String, List<FileInfoVO>> result, String line, Boolean isReadStatus, String[] path) {
		
		if(Boolean.TRUE.equals(isDirectyIndex(line))) {
			path[0] = line;
			result.putIfAbsent(line, new ArrayList<>());
		} else {
			processReadFileLine(result, path[0], line, isReadStatus);
		}		
	}
	
	private Boolean isDirectyIndex(String line) {
		return line.charAt(0) != ConstantUtil.TAB.charAt(0);
	}
	
	private void processReadFileLine(Map<String, List<FileInfoVO>> result, String path, String line, Boolean isReadStatus) {
		String[] fileInfo = line.split(ConstantUtil.TAB);
		
		if (fileInfo.length == 5) {
			FileInfoVO fileVO  = createFileInfoVO(fileInfo, path, isReadStatus);
			result.get(path).add(fileVO);
		}		
	}
	
	private FileInfoVO createFileInfoVO(String[] fileInfo, String path, Boolean isReadStatus ) {
        FileInfoVO fileVO = new FileInfoVO();
        fileVO.setName(fileInfo[1]);
        fileVO.setSize(Long.parseLong(fileInfo[2]));
        fileVO.setTimestamp(Long.parseLong(fileInfo[3]));
        fileVO.setPath(path);
        fileVO.setStatus((Boolean.TRUE.equals(isReadStatus)) ? fileInfo[4] : ConstantUtil.DELETE_STATUS);
        return fileVO;
    }
	
	public void proccessIndexFiles(String directory, FileInfoVO fileInfo) {
		List<FileInfoVO> directoryFiles = this.indexFiles.computeIfAbsent(directory, k -> new ArrayList<>());
		manageIndexFileStatus(directoryFiles, fileInfo);
	}
	
	private void manageIndexFileStatus(List<FileInfoVO> list, FileInfoVO fileInfo) {
		Optional<FileInfoVO> matchedFile = list.stream().filter(e -> e.getName().equals(fileInfo.getName())).findFirst();
		if(Boolean.FALSE.equals(matchedFile.isPresent())) {
			fileInfo.setStatus(ConstantUtil.INSERT_STATUS);
			list.add(fileInfo);
		} else {
			FileInfoVO foundFile = matchedFile.get();
			Long size = foundFile.getSize();
			Long time = foundFile.getTimestamp();
			if(size.equals(fileInfo.getSize()) && time.equals(fileInfo.getTimestamp())) {
				foundFile.setStatus(ConstantUtil.NO_CHANGE_STATUS);
			} else {
				fileInfo.setStatus(ConstantUtil.UPDATE_STATUS);
				list.remove(foundFile);
				list.add(fileInfo);				
			}
		}
	}
}
