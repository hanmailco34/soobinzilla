package com.soobin.soobinzilla.exception.code;

import com.soobin.soobinzilla.exception.IFileTransferError;

import lombok.Getter;

@Getter
public enum TokenErrorCode implements IFileTransferError {
	NOT_FORMAT(70000, "토큰 형식에 맞지 않아요."),
	NOT_SIGNATURE(70001, "토큰 시그니처가 다르네요."),
	EXPIRE_TIME(70002, "유효 시간이 지나버렸네요."),
	KEY_LENGTH_OVER(70003, "INDEX 길이가 너무 길어요."),
	NOT_ACTIVE(70004, "비활성화된 토큰이네요."),
	;

	private final Integer	code;
	private final String	description;
	
	TokenErrorCode(Integer code, String desc) {
		this.code = code;
		this.description = desc;
	}
}
