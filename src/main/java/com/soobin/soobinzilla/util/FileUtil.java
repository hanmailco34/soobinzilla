package com.soobin.soobinzilla.util;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.DirectoryStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.util.zip.GZIPInputStream;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.FileErrorCode;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;

import lombok.experimental.UtilityClass;

@UtilityClass
public class FileUtil {
	
	public static void makeDir(String folderPath) {
		File folder = new File(folderPath);
		if(!folder.exists()) {
			folder.mkdirs();
		}
	}
	
	public static void writeFile(String directory, String fileName, InputStream is) throws FileTransferException {
		String path = PathUtil.convertPath(directory);
		makeDir(path);
		path = PathUtil.addFilePath(path, fileName);
		
		try(
				FileOutputStream fos = new FileOutputStream(path);
			) {
			try (BufferedInputStream reader = new BufferedInputStream(is);) {
				int n;
		        byte[] bytes = new byte[ConstantUtil.BUFFER_SIZE];
		        while ((n = reader.read(bytes)) > 0) {
		        	fos.write(bytes, 0, n);
		        }
			}
		} catch (FileNotFoundException e) {
			throw new FileTransferException(FileErrorCode.NOT_EXIST_FILE);
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.WRITE_ERROR);
		}
	}
	
	public static void writeIndex(String rootPath, List<FileInfoVO> fileList) throws FileTransferException {
		makeDir(PathUtil.convertPath(rootPath));
		String filePath = PathUtil.convertPath(rootPath + ConstantUtil.FILE_SEPARATOR + ConstantUtil.INDEX_FILE_NAME);
		Map<String, List<FileInfoVO>> groupFileList = fileList.stream().collect(Collectors.groupingBy(FileInfoVO::getPath));
		try(BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filePath), StandardCharsets.UTF_8))) {
			for (Entry<String, List<FileInfoVO>> entry : groupFileList.entrySet()) {
				String path = entry.getKey();
				List<FileInfoVO> files = entry.getValue();
				StringBuilder sb = new StringBuilder(path+ConstantUtil.ENTER);
				for(FileInfoVO file : files) {
					sb.append(ConstantUtil.TAB).append(file.getName())
						.append(ConstantUtil.TAB).append(file.getSize())
						.append(ConstantUtil.TAB).append(file.getTimestamp())
						.append(ConstantUtil.TAB).append(file.getStatus())
						.append(ConstantUtil.ENTER);
				}
				bw.write(sb.toString());
			}
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.WRITE_ERROR);
		} 
	}
	
	public static List<Path> listFilesInDirectory(String path) throws FileTransferException {				
		try {
			List<Path> fileList = new ArrayList<>();
			Files.walkFileTree(Paths.get(path), new SimpleFileVisitor<Path>() {
				@Override
				public FileVisitResult visitFile(Path path, BasicFileAttributes attrs) throws IOException {
					fileList.add(path);
					return FileVisitResult.CONTINUE;
				}			
			});
			return fileList;
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.READ_ERROR);
		}  
	}
	
	public static Long getFileTimeStamp(String filePath) throws FileTransferException {		
		try {
			Path path = Paths.get(filePath);
	        BasicFileAttributes attrs = Files.readAttributes(path, BasicFileAttributes.class);
			return attrs.lastModifiedTime().toMillis();
		} catch (IOException e) {
			throw new FileTransferException(FileErrorCode.READ_ERROR);
		}        
	}
	
	public static void delete(String path) throws FileTransferException {
		String filePath = PathUtil.convertPath(path);
		Path file = Paths.get(filePath);
		
		if (Files.exists(file)) {
	        deleteRecursively(file);
	    }
	}
	
	private static void deleteRecursively(Path file) throws FileTransferException {
	    if (Files.isDirectory(file)) {
	    	try(DirectoryStream<Path> ds = Files.newDirectoryStream(file)) {
				for(Path path : ds) {
					deleteRecursively(path);
				}
			} catch (IOException e) {
				throw new FileTransferException(FileErrorCode.READ_ERROR);
			}
	    }
	    
	    if (!ConstantUtil.INDEX_FILE_NAME.equals(file.getFileName().toString())) {
	    	try {
				Files.delete(file);
			} catch (IOException e) {
				if(!(e instanceof DirectoryNotEmptyException)) {
					String filePath = String.format("파일 이름 : %s", file.getFileName().toString());
					throw new FileTransferException(FileErrorCode.NOT_DELETE_FILE, filePath);
				}
			}
	    }
	}
	
	public static List<String> readFile(String filePath) throws FileTransferException {
        Path path = getPath(filePath);
        List<String> result = new ArrayList<>();
        try (BufferedReader reader = Files.newBufferedReader(path, StandardCharsets.UTF_8)) {
            String line;
            while ((line = reader.readLine()) != null) {
            	result.add(line);
            }
        } catch (IOException e) {
        	throw new FileTransferException(FileErrorCode.READ_ERROR);
        }
        
        return result;
	}
	
	public static List<String> readGzipFile(String filePath) throws FileTransferException {
        Path path = getPath(filePath);
        List<String> result = new ArrayList<>();
        try (GZIPInputStream gzipInputStream = new GZIPInputStream(Files.newInputStream(path));
    		BufferedReader reader = new BufferedReader(new InputStreamReader(gzipInputStream, StandardCharsets.UTF_8))) {
            String line;
            while ((line = reader.readLine()) != null) {
            	result.add(line);
            }
        } catch (IOException e) {
        	throw new FileTransferException(FileErrorCode.READ_ERROR);
        }
        
        return result;
	}
	
	private static Path getPath(String filePath) throws FileTransferException {
		Path path = Paths.get(filePath);
        if (!Files.exists(path)) {
        	throw new FileTransferException(FileErrorCode.NOT_EXIST_FILE);
        }
        return path;
	}

}
