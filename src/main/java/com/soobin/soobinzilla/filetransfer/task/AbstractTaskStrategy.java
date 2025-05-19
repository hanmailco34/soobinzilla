package com.soobin.soobinzilla.filetransfer.task;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;

public abstract class AbstractTaskStrategy<T> {

	protected abstract void excuteTask(T client, Long connectionId, FileInfoVO fileInfoVO, String localDirectory, String serverDirectory) throws FileTransferException;
	
//	protected void excuteFail(FileTransferException e, FileInfoVO fileInfoVO) {
//		logger.error("에러 발생발생 파일 : {}", fileInfoVO);
//		FileTransferExceptionHandler.handleException(e);
//	}
}
