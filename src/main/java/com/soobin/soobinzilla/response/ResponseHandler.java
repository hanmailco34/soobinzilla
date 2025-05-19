package com.soobin.soobinzilla.response;

import org.springframework.stereotype.Component;

import com.soobin.soobinzilla.util.ConstantUtil;

@Component
public class ResponseHandler {
	
	public <T> ResponseApi<T> generateResponse(T responseData) {
		ResponseDto<T> response = ResponseDto.<T>builder()
				.status(ConstantUtil.STATUS_OK)
				.data(responseData)
				.build();
		return new ResponseApi<>(response);
	}
	
	public <T> ResponseApi<T> exceptionResponse(T responseData) {
		ResponseDto<T> response = ResponseDto.<T>builder()
				.status(ConstantUtil.STATUS_OOPS)
				.data(responseData)
				.build();
		return new ResponseApi<>(response);
	}
	
}
