package com.soobin.soobinzilla.util;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import lombok.experimental.UtilityClass;

@UtilityClass
public class SecurityUtil {
	// URL
	public static final String LIST_URL = "/list";
	public static final String INSERT_URL = "/insert";
	public static final String UPDATE_URL = "/update";
	public static final String DELETE_URL = "/delete";
	private static final String ALL_URL = "/**";
	
	public static final String TEST_URL = "/test";
	private static final String TEST_ALL_URL = TEST_URL + ALL_URL;
	
	public static final String USER_URL = "/user";
	private static final String USER_INSERT_URL = USER_URL + INSERT_URL;
	private static final String USER_ALL_URL = USER_URL + ALL_URL;
	
	public static final String USERS_URL = "/users";
	private static final String USERS_ALL_URL = USERS_URL + ALL_URL;
		
	public static final String DEPARTMENT_URL = "/department";
	private static final String DEPARTMENT_LIST_URL = DEPARTMENT_URL + LIST_URL;
	private static final String DEPARTMENT_INSERT_URL = DEPARTMENT_URL + INSERT_URL;
	private static final String DEPARTMENT_UPDATE_URL = DEPARTMENT_URL + UPDATE_URL;
	private static final String DEPARTMENT_DELETE_URL = DEPARTMENT_URL + DELETE_URL;	
	
	public static final String AUTH_URL = "/auth";
	public static final String LOGIN_URL = "/login";
	public static final String REFRESH_URL = "/refresh";
	public static final String INFO_URL = "/info";
	private static final String AUTH_LOGIN_URL = AUTH_URL + LOGIN_URL;
	public 	static final String AUTH_REFRESH_URL = AUTH_URL + REFRESH_URL;
	private static final String AUTH_INFO_URL = AUTH_URL + INFO_URL;
	
	public static final String PERMISSION_URL = "/permission";
	public static final String READ_DIRECTORY_URL = "/readDirectory";
	private static final String PERMISSION_INSERT_URL = PERMISSION_URL + INSERT_URL;
	private static final String PERMISSION_LIST_URL = PERMISSION_URL + LIST_URL;
	private static final String PERMISSION_READ_DIRECTORY_URL = PERMISSION_URL + READ_DIRECTORY_URL;
	
	public static final String SCHEDULE_URL = "/schedule";
	private static final String SCHEDULE_ALL_URL = SCHEDULE_URL + ALL_URL;
	
	public static final String CONNECTION_URL = "/connection";
	private static final String CONNECTION_ALL_URL = CONNECTION_URL + ALL_URL;
	
	// 권한 설정
	public static final List<String> PERMIT_ALL_URLS = Collections.unmodifiableList(Arrays.asList(
		TEST_ALL_URL, USER_INSERT_URL, AUTH_LOGIN_URL
	));
	
	public static final List<String> MANAGER_URLS = Collections.unmodifiableList(Arrays.asList(
		PERMISSION_INSERT_URL, PERMISSION_LIST_URL, SCHEDULE_ALL_URL, CONNECTION_ALL_URL, USERS_ALL_URL
	));
	
	public static final List<String> ROLE_ADMIN_URLS = Collections.unmodifiableList(Arrays.asList(
		DEPARTMENT_INSERT_URL, DEPARTMENT_DELETE_URL, DEPARTMENT_UPDATE_URL, AUTH_INFO_URL
	));
	
	public static final List<String> ROLE_ALL_URLS = Collections.unmodifiableList(Arrays.asList(
		USER_ALL_URL, AUTH_REFRESH_URL, DEPARTMENT_LIST_URL, PERMISSION_URL, PERMISSION_READ_DIRECTORY_URL
	));
}
