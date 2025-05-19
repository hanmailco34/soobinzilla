package com.soobin.soobinzilla.exception.code;

import com.soobin.soobinzilla.exception.IFileTransferError;

import lombok.Getter;

@Getter
public enum ConnectionErrorCode implements IFileTransferError {	
	UNSUPPORTED_PROTOCOL(40000, "지원하지 않는 프로토콜입니다."),
	CONNECTION_ERROR(40001, "서버에 연결할 수 없습니다.서버와 포트를 확인해주세요."),
	INVALID_PORT(40002, "프로토콜에 맞는 포트를 확인해주세요."),
	PORT_OUT_OF_RANGE(40003, "포트가 유효한 범위를 벗어났어요."),
	CONNECTION_TIMEOUT(40004, "연결 시간이 초과했습니다."),
	CONNECTION_FAILED(40005, "서버 연결에 실패했습니다."),
	LOGIN_ERROR(40006, "로그인이 되지 않습니다. 아이디와 비밀번호를 확인해주세요."),
	SMB_AUTHENTICATION_ERROR(40007, "SMB 호스트와 포트를 확인해주세요. 또는 비밀번호를 확인해주세요"),
	SFTP_UNKOWN_HOST_KEY(40008, "SFTP 호스트 키를 알 수 없어요."),
	SMB_NOT_EXIST_SHARE(40009, "SMB 공유할 섹션 이름이 존재하지 않아요. 또는 아이디를 확인해주세요"),
	INVALID_PARAMETER(40009, "매개변수 값이 null 이거나 비어있습니다."),
	REJECT_HOSTKEY(40010, "서버 측에 등록된 호스트가 아닙니다."),
	NOT_COMPLETE_PENDING(40011, "FTP 명령어가 완료되지 않았습니다."),
	THREAD_INTERRUPT(40012, "스레드에 인터럽트가 걸려버렸네요."),
	SESSION_DOWN(40013, "세션이 연결되지 않았어요."),
	SERVER_ERROR(40014, "파일 전송 서버의 문제가 생겼습니다."),
	;

	private final Integer	code;
	private final String	description;
	
	ConnectionErrorCode(Integer code, String desc) {
		this.code = code;
		this.description = desc;
	}
}
