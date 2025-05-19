package com.soobin.soobinzilla.exception.code;

import com.soobin.soobinzilla.exception.IFileTransferError;

import lombok.Getter;

@Getter
public enum FileErrorCode implements IFileTransferError {
	READ_ERROR(40100, "파일을 읽을 수가 없어요."),
	WRITE_ERROR(40101, "파일을 쓸 수가 없어요."),
	INVALID_INDEX_FILE(40102, "인덱스 파일 라인 형식이 달라졌어요."),
	NOT_ALGORITHM(40103, "알고리즘 형식이 아니예요."),
	NULL_ERROR(40104, "파라미터 값이 NULL이 왔어요."),
	NOT_EXIST_FILE(40105, "파일이 존재하지 않아요."),
	ACCESS_DENIED(40106, "파일에 접근 권한이 없어요."),
	NOT_CHANGE_DIRECTORY(40107, "폴더를 바꿀 수가 없어요."),
	NOT_MAKE_DIRECTORY(40108, "폴더를 만들 수가 없어요."),
	NOT_CHMOD_FILE(40109, "파일 권한 변경을 실패했어요."),
	NOT_DELETE_FILE(40110, "삭제할 파일이 존재하지 않아요."),
	UPLOAD_ERROR(40111, "업로드에 문제가 생겼어요."),
	ENCODING_ERROR(40112, "인코딩 환경에 문제가 있어요"),
	;

	private final Integer	code;
	private final String	description;
	
	FileErrorCode(Integer code, String desc) {
		this.code = code;
		this.description = desc;
	}
}
