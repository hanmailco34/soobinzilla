package com.soobin.soobinzilla.filetransfer.task.strategy;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.filetransfer.core.AbstractFileTransferHandler;
import com.soobin.soobinzilla.filetransfer.task.AbstractTaskStrategy;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;

public class UploadStrategy<T> extends AbstractTaskStrategy<T> {

	AbstractFileTransferHandler<T> handler;
	
	public UploadStrategy(AbstractFileTransferHandler<T> handler) {
		this.handler = handler;
	}

	@Override
	protected void excuteTask(T client, Long connectionId, FileInfoVO fileInfoVO, String localDirectory, String serverDirectory)
			throws FileTransferException {
		this.handler.uploadHandler(client, connectionId, fileInfoVO, localDirectory, serverDirectory, false);
	}
}