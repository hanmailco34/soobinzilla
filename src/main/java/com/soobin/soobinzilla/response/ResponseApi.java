package com.soobin.soobinzilla.response;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

public class ResponseApi<T> extends ResponseEntity<ResponseDto<T>> {

	public ResponseApi(ResponseDto<T> body) {
		super(body, HttpStatus.OK);
	}
}
