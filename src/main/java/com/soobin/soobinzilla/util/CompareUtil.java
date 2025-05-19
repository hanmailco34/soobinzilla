package com.soobin.soobinzilla.util;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.FileErrorCode;

public class CompareUtil {
	public static String calculateHash(InputStream inputStream, String algorithm) throws FileTransferException {
		try {
			MessageDigest md = MessageDigest.getInstance(algorithm);
			
			byte[] bytes = new byte[ConstantUtil.BUFFER_SIZE];
	        int reader;
	        while ((reader = inputStream.read(bytes)) != -1) {
	            md.update(bytes, 0, reader);
	        }
			
	        return ParseUtil.bytesToHex(md.digest());
	        
		} catch (NoSuchAlgorithmException e) {
			throw new FileTransferException(FileErrorCode.NOT_ALGORITHM);
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.READ_ERROR);
		} catch (NullPointerException e) {
			throw new FileTransferException(FileErrorCode.NULL_ERROR);				
		}		
	}
	
	public static String calculateMD5FromPath(String filePath) throws FileTransferException {
		try (InputStream inputStream = new FileInputStream(filePath)) {
			return calculateHash(inputStream, ConstantUtil.ALGORITHM_MD5);
		} catch (FileNotFoundException e) {
			throw new FileTransferException(FileErrorCode.NOT_EXIST_FILE);
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.READ_ERROR);
		}
	}
	
	public static Boolean compareFileNameWithSuffix(String fileName1, String fileName2, String suffix) {
		String baseName1 = getBaseFileName(fileName1, suffix);
        String baseName2 = getBaseFileName(fileName2, suffix);
        return baseName1.equals(baseName2);
	}
	
	public static String getBaseFileName(String fileName, String suffix) {
		int suffixIndex = fileName.indexOf(suffix);
        int dotIndex = fileName.indexOf('.');
        
        if (suffixIndex != -1) {
            return fileName.substring(0, suffixIndex);
        } else if (dotIndex != -1) {
            return fileName.substring(0, dotIndex);
        } else {
            return fileName;
        }
	}
}
