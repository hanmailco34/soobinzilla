package com.soobin.soobinzilla.response;

import com.soobin.soobinzilla.exception.IFileTransferError;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ResponseException {
	private Integer code;
	private String description;
	
	public ResponseException(IFileTransferError e) {
		this.code = e.getCode();
		this.description = e.getDescription();
	}
}
