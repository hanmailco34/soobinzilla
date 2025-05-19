package com.soobin.soobinzilla.exception.handler;

import javax.servlet.http.HttpServletRequest;

import org.springframework.data.mapping.PropertyReferenceException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.DBErrorCode;
import com.soobin.soobinzilla.response.ResponseApi;
import com.soobin.soobinzilla.response.ResponseException;
import com.soobin.soobinzilla.response.ResponseHandler;
import com.soobin.soobinzilla.util.LogUtil;

import lombok.RequiredArgsConstructor;

@RestControllerAdvice
@RequiredArgsConstructor
public class GlobalExceptionHandler {
	
	private final ResponseHandler responseHandler;

	@ExceptionHandler(FileTransferException.class)
	public ResponseApi<ResponseException> handleFileTransferException(FileTransferException e, HttpServletRequest request) {
		LogUtil.error(e);
		
		return responseHandler.generateResponse(new ResponseException(e.getError()));
	}
	
	@ExceptionHandler(PropertyReferenceException.class)
	public ResponseApi<ResponseException> handlePropertyReferenceException(PropertyReferenceException error, HttpServletRequest request) {
		FileTransferException e = new FileTransferException(DBErrorCode.NOT_PROPERTY,error.getMessage());
		return handleFileTransferException(e, request);
	}
}
