package com.soobin.soobinzilla.filetransfer.vo;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FileInfoVO {
	private String	path;
	private String	name;
	private Long	size;
	private Long	timestamp;
	private String	status;
}
