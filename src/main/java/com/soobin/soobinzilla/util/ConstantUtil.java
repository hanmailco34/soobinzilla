package com.soobin.soobinzilla.util;

import lombok.experimental.UtilityClass;

@UtilityClass
public class ConstantUtil {
	
	// COOKIE NAME
	public static final String ACCESS_TOKEN_COOKIE = "ATC";
	public static final String REFRESH_TOKEN_COOKIE = "RTC";
	
	// TOKEN TYPE
	public static final String ACCESS_TOKEN = "ACCESS";
	public static final String REFRESH_TOKEN = "REFRESH";
	
	// USER INFO UPDATE TYPE
	public static final String INFO_UPDATE = "info";
	public static final String PASSWORD_UPDATE = "password";
	public static final String ROLE_UPDATE = "role";
	public static final String DEPARTMENT_UPDATE = "department";

	// SECURITY AUTH
	public static final String ADMIN = "ADMIN";
	public static final String MEMBER = "MEMBER";	
	
	// STATUS
	public static final String STATUS_OOPS = "OOPS";
	public static final String STATUS_OK = "OK";
	
	// ALGORITHM
	public static final String ALGORITHM_MD5 = "md5";
	
	// VALIDATE LEN
	public static final int	USER_ID_LENGTH = 30;
	public static final int	PASSWORD_LENGTH = 128;
	public static final int	NAME_LENGTH = 30;
	
	public static final int TOKEN_LENGTH = 500;
	
	// VALIDATE PATTERN
	public static final String USER_ID_PATTERN = "^(?!.*[ㄱ-ㅎㅏ-ㅣ가-힣]).*$";
	public static final String PASSWORD_PATTERN = "^(?=.*[0-9])(?=.*[a-zA-Z])(?=.*[@#$%^&+=!])(?=\\S+$).*$";
	public static final String CONNECTION_DATE_LOG_PATTERN = "yyyy-MM-dd";
	
	// VALID PAGE
	public static final String DESC = "desc";
	public static final String ASC = "asc";
	
	// Connection 로그
	public static final String SUCCESS_CONNECT = "우주 전파를 타고 서버의 연결되었습니다.";
	public static final String SUCCESS_LOGIN = "비밀번호 확인을 통과하여 서버의 게이트가 활짝 열렸습니다.";
	public static final String SUCCESS_DISCONNECT = "닌자처럼 은밀하고 순식간에 로그아웃했습니다. 다음에 만나요.";
	
	// Connection thread pool
	public static final Integer	THREAD_CORE_SIZE = 10;
	public static final Integer	THREAD_MAX_POOL_SIZE = 50;
	public static final Integer	THREAD_KEEP_ALIVE_TIME = 10;
	public static final Integer	THREAD_QUEUE_SIZE = 20;
	
	// FILE TRANSPER
	public static final String 	FILE_SEPARATOR = "/";
	public static final String 	WINDOW_SEPARATOR = "\\";
	
	public static final Integer BUFFER_SIZE = 4096;
	
	public static final String	MERGE_WRITE_MODE="MERGE_WRITE";
	public static final String	RESET_WRITE_MODE="RESET_WRITE";
	
	public static final String	DELETE_SYNC_MODE="DELETE_SYNC";
	public static final String	DELETE_ALL_MODE="DELETE_ALL";
	
	public static final String	INDEX_FILE_NAME = "index.lsb";
	public static final String 	NO_CHANGE_STATUS = "NC";
	public static final String 	NO_DOWNLOAD_STATUS = "ND";
	public static final String 	INSERT_STATUS = "I";
	public static final String 	UPDATE_STATUS = "U";
	public static final String 	DELETE_STATUS = "D";
	
	public static final String	TAB = "\t";
	public static final String	ENTER = "\n";
}
