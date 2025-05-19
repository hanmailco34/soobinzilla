package com.soobin.soobinzilla.util;

import java.nio.file.Path;
import java.nio.file.Paths;

import lombok.experimental.UtilityClass;

@UtilityClass
public class PathUtil {
	public static boolean isWindow() {
		String osName = System.getProperty("os.name").toLowerCase();
		return osName.indexOf("win") >= 0;
	}
	
	public static String addFilePath(String path, String fileName) {
		if(isWindow()) {
			path = path.replace(ConstantUtil.FILE_SEPARATOR, ConstantUtil.WINDOW_SEPARATOR);
			path += ConstantUtil.WINDOW_SEPARATOR + fileName;
		} else {
			path += ConstantUtil.FILE_SEPARATOR + fileName;
		}
		return path;
	}
	
	public static String convertPath(String path) {
		if(isWindow()) path = path.replace(ConstantUtil.FILE_SEPARATOR, ConstantUtil.WINDOW_SEPARATOR);
		return path;
	}
	
	public static String getLocalFilePath(String localDirectory, String filePath, String fileName) {
		String separator = ConstantUtil.FILE_SEPARATOR;
		if(filePath.endsWith("/")) separator = "";
		return convertPath(localDirectory + filePath + separator + fileName);
	}
	
	public static String getUploadFilePath(String filePath, String fileName) {
		String separator = ConstantUtil.FILE_SEPARATOR;
		if(filePath.endsWith("/")) separator = "";
		return filePath + separator + fileName;
	}
	
	public static String getRealativePath(String basePath, String fullPath) {
		Path base = Paths.get(basePath).toAbsolutePath().normalize();
        Path full = Paths.get(fullPath).toAbsolutePath().normalize();
        Path relativePath = base.relativize(full);
        return ConstantUtil.FILE_SEPARATOR + relativePath.toString().replace(ConstantUtil.WINDOW_SEPARATOR, ConstantUtil.FILE_SEPARATOR);
	}
}
