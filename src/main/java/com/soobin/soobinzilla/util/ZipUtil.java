package com.soobin.soobinzilla.util;

import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.FileErrorCode;

import lombok.experimental.UtilityClass;

@UtilityClass
public class ZipUtil {
	public static Boolean isZipFile(String filePath, String fileName) throws FileTransferException {
		return isZipFileByExtension(fileName) && isZipFileByHeader(filePath, fileName);
	}

	public static Boolean isZipFileByExtension(String fileName) {
	    return fileName.toLowerCase().endsWith(".zip");
	}
	
	private static Boolean isZipFileByHeader(String filePath, String fileName) throws FileTransferException {
		String path = PathUtil.convertPath(filePath + ConstantUtil.FILE_SEPARATOR + fileName);
	    try (FileInputStream fis = new FileInputStream(path)) {
	        byte[] header = new byte[4];
	        if (fis.read(header) == 4) {
	            return (header[0] == 'P' && header[1] == 'K');
	        }
	    } catch (IOException e) {
	    	throw new FileTransferException(FileErrorCode.READ_ERROR, e.getMessage());
	    }
	    return false;
	}
	
	public static void unzip(String filePath, String fileName) throws FileTransferException {
		String zipFilePath = PathUtil.convertPath(filePath + ConstantUtil.FILE_SEPARATOR + fileName);
		String directoryName = getDirectoryname(fileName);
		String directoryPath = PathUtil.convertPath(filePath + ConstantUtil.FILE_SEPARATOR + directoryName);		
		
		unzipAndDest(zipFilePath, directoryPath);
		FileUtil.delete(zipFilePath);		
	}
	
	public static void unzipAndDest(String zipFilePath, String destDirectory) throws FileTransferException {
		FileUtil.makeDir(destDirectory);
		try (ZipInputStream zis = new ZipInputStream(new FileInputStream(zipFilePath))) {
			ZipEntry entry;
            while ((entry = zis.getNextEntry()) != null) {
            	String filePath = PathUtil.convertPath(destDirectory + ConstantUtil.FILE_SEPARATOR + entry.getName());
            	
            	if(Boolean.TRUE.equals(entry.isDirectory())) {
            		FileUtil.makeDir(filePath);
            	} else {
            		extractFile(zis, filePath);
            	}
            	
            	if(Boolean.TRUE.equals(isZipFileByExtension(filePath))) {
            		unzip(destDirectory, entry.getName());
            	}
            	
            	zis.closeEntry();
            }
		} catch (FileNotFoundException e) {
			throw new FileTransferException(FileErrorCode.NOT_EXIST_FILE);
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.WRITE_ERROR, e.getMessage());
		}
	}
		
	public static String getDirectoryname(String fileName) {
		return fileName.substring(0,fileName.lastIndexOf("."));
	}
	
	private static void extractFile(ZipInputStream zis, String filePath) throws IOException {
		try (BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(filePath))) {
			byte[] buffer = new byte[ConstantUtil.BUFFER_SIZE];
			int bytesRead;
            while ((bytesRead = zis.read(buffer)) != -1) {
                bos.write(buffer, 0, bytesRead);
            }
		}
	}
}
