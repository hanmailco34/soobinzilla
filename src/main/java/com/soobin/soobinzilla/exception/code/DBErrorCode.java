package com.soobin.soobinzilla.exception.code;

import com.soobin.soobinzilla.exception.IFileTransferError;

import lombok.Getter;

@Getter
public enum DBErrorCode implements IFileTransferError {
	NOT_FOUND(60000, "데이터를 찾을 수가 없어요."),
	DUPLICATE_ENTITY(60001, "중복된 내용이 있어요."),
	LOOP_ENTITY(60002, "순환 참조가 발생해 버려요."),
	LENGTH_OVER(60003, "길이가 너무 길어요."),
	NOT_PATTERN(60004, "규칙이 충족하지 않아요."),
	NOT_NULL(60005, "값이 없네요 넣어주세요."),
	NOT_PROPERTY(60006, "컬럼 값이 없어요. 똑바로 넣어주세요."),
	NOT_PAGE(60007, "자연수가 좋아요."),
	NOT_ORDER(60008, "오르락 내리락만 지정할 수 있어요."),
	;

	private final Integer	code;
	private final String	description;
	
	DBErrorCode(Integer code, String desc) {
		this.code = code;
		this.description = desc;
	}
}
