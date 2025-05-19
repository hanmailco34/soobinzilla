package com.soobin.soobinzilla.exception;

import lombok.Getter;

@Getter
public class FileTransferException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = -4500850655999696690L;
	private final Integer code;
	private final IFileTransferError error;

	public FileTransferException(IFileTransferError error) {
		super(error.getDescription());
		this.code = error.getCode();
		this.error = error;
	}
	
	public FileTransferException(IFileTransferError error, String message) {
		super(error.getDescription() + message);
		this.code = error.getCode();
		this.error = error;
	}
	
	public FileTransferException(Exception e) {
		super(e.getMessage());
		this.code = e.hashCode();
		this.error = null;
	}
}
