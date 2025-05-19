package com.soobin.soobinzilla.filetransfer.core;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;

public interface IFileTransferHandler<T> {
	void downloadHandler(T client, Long connectionId, FileInfoVO file, String localDirectory, Boolean isThread) throws FileTransferException ;
	
	void uploadHandler(T client, Long connectionId, FileInfoVO file, String localDirectory, String serverDirectory, Boolean isThread) throws FileTransferException ;
	
	void deleteHandler(T client, Long connectionId, FileInfoVO file, String serverDirectory, Boolean isThread) throws FileTransferException ;
}
