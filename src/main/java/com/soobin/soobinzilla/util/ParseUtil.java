package com.soobin.soobinzilla.util;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;

import lombok.experimental.UtilityClass;

@UtilityClass
public class ParseUtil {

	public static String bytesToHex(byte[] bytes) { 
		StringBuilder sb = new StringBuilder(); 
		for (byte b : bytes) { 
			sb.append(String.format("%02x", b)); 
		} 
		return sb.toString();
	}
	
	public static List<FileInfoVO> mapToListFileInfo(Map<String, List<FileInfoVO>> fileListMap) {
		return fileListMap.values().stream()
				.flatMap(Collection::stream)
				.collect(Collectors.toList());
	}
	
	public static List<FileInfoVO> mergeFileList(List<FileInfoVO> originList, List<FileInfoVO> newList) {
		Map<String, FileInfoVO> result = new HashMap<>();
		
		originList.forEach(fileVO -> result.put(setFileKey(fileVO), fileVO));
		
		newList.forEach(fileVO -> result.put(setFileKey(fileVO), fileVO));
		
		return new ArrayList<>(result.values());
	}
	
	public static FileInfoVO toFileInfo(String path, File file) throws FileTransferException {
		FileInfoVO fileInfo = new FileInfoVO();
		fileInfo.setPath(path);
		fileInfo.setName(file.getName());
		fileInfo.setSize(file.length());
		fileInfo.setTimestamp(FileUtil.getFileTimeStamp(file.getAbsolutePath()));
		fileInfo.setStatus(ConstantUtil.INSERT_STATUS);
		
		return fileInfo;
	}
	
	private static String setFileKey(FileInfoVO fileInfoVO) {
		return fileInfoVO.getPath() + ConstantUtil.FILE_SEPARATOR + fileInfoVO.getName();
	}
}
