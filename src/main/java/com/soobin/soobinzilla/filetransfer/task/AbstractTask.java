package com.soobin.soobinzilla.filetransfer.task;

import com.soobin.soobinzilla.exception.FileTransferException;

public abstract class AbstractTask<T> implements Runnable {

	protected TaskVO<T> taskVO;
	
	protected AbstractTask(TaskVO<T> taskVO) {
		this.taskVO = taskVO;
	}
	
	protected abstract void excuteTask() throws FileTransferException;
	
	@Override
	public void run() {
		try {
			excuteTask();
		} catch (FileTransferException e) {
			// [TODO] log 관리.
			e.printStackTrace();
		}
	}
}
