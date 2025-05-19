package com.soobin.soobinzilla.exception.code;

import org.springframework.http.HttpStatus;

import com.soobin.soobinzilla.exception.IFileTransferError;

import lombok.Getter;

@Getter
public enum SecurityErrorCode implements IFileTransferError {
	SECURITY_UNAUTHORIZED(HttpStatus.UNAUTHORIZED.value(), "인증에 실패하였어요."),
	SECURITY_FORBIDDEN(HttpStatus.FORBIDDEN.value(), "권한이 있는 사람만 접속할 수 있어요. 당신은 권한이 없네요."),
	SECURITY_DEPARTMENT(HttpStatus.FORBIDDEN.value(), "같은 부서에 있는 사람만 통제할 수 있어요. 당신은 권한이 없네요."),
	SECURITY_ADMIN(HttpStatus.FORBIDDEN.value(), "어드민은 어드민만이 건드릴 수 있는 신성한 영역입니다."),
	SECURITY_TYPE(HttpStatus.FORBIDDEN.value(), "타입을 똑바로 입력하라구요~! 키보드 앞에서 춤추지 말고, 제발 정확한 글자로 마음을 전해주세요."),
	;

	private final Integer	code;
	private final String	description;
	
	SecurityErrorCode(Integer code, String desc) {
		this.code = code;
		this.description = desc;
	}
}
