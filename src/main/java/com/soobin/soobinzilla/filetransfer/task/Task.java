package com.soobin.soobinzilla.filetransfer.task;

import com.soobin.soobinzilla.exception.FileTransferException;

public class Task<T> extends AbstractTask<T> {
	
	public Task(TaskVO<T> taskVO) {
		super(taskVO);
	}

	@Override
	protected void excuteTask() throws FileTransferException {
		super.taskVO.getTaskStrategy().excuteTask(super.taskVO.getClient(), super.taskVO.getConnectionId(), super.taskVO.getFileInfoVO(), super.taskVO.getLocalDirectory(), super.taskVO.getServerDirectory());		
	}

}
