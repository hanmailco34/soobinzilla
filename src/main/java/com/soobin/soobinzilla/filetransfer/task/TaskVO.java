package com.soobin.soobinzilla.filetransfer.task;

import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TaskVO<T> {
	private T						client;
	private	Long					connectionId;
	private String					localDirectory;
	private String					serverDirectory;
	private FileInfoVO 				fileInfoVO;
	private AbstractTaskStrategy<T>	taskStrategy;
}
