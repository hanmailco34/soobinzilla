package com.soobin.soobinzilla.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.soobin.soobinzilla.dto.auth.AuthInfo;
import com.soobin.soobinzilla.dto.auth.AuthPayload;
import com.soobin.soobinzilla.dto.request.UserLoginDto;
import com.soobin.soobinzilla.dto.response.LoginDto;
import com.soobin.soobinzilla.dto.response.UserDto;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.response.ResponseApi;
import com.soobin.soobinzilla.response.ResponseHandler;
import com.soobin.soobinzilla.service.AuthTokenService;
import com.soobin.soobinzilla.service.UserService;
import com.soobin.soobinzilla.util.AuthUtil;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.CookieUtil;
import com.soobin.soobinzilla.util.CryptoUtil;
import com.soobin.soobinzilla.util.IPUtil;
import com.soobin.soobinzilla.util.SecurityUtil;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

@Tag(name = "인증 컨트롤러", description = "토큰을 이용한 인증 방식")
@RestController
@RequestMapping("${app.api.prefix}" + SecurityUtil.AUTH_URL)
@RequiredArgsConstructor
public class AuthController {
	
	private final UserService userService;
	
	private final AuthTokenService loginService;

	private final ResponseHandler responseHandler;
	
	private final CryptoUtil cryptoUtil;
	
	private final AuthUtil authUtil;
	
	@Tag(name = "인증 컨트롤러", description = "토큰을 이용한 인증 방식")
	@Operation(summary = "로그인", description = "유저아이디와 비밀번호를 이용한 로그인")
	@PostMapping(SecurityUtil.LOGIN_URL)
	public ResponseApi<Boolean> login(@RequestBody UserLoginDto requestDto, HttpServletRequest request, HttpServletResponse response) throws FileTransferException {
		// 패스워드 유효성 확인
		userService.validPassword(requestDto.getPassword());
		// 유저정보 가져오기
		UserDto userDto = userService.login(requestDto.getUserId(), cryptoUtil.sha512((requestDto.getPassword())));	
		
		LoginDto loginDto = loginService.login(userDto.getId(), userDto.getRole());
		
		CookieUtil actCookie = CookieUtil.sessionCookie(ConstantUtil.ACCESS_TOKEN_COOKIE, loginDto.getAccessToken());
		actCookie.addCookie(response);
		
		CookieUtil refCookie = CookieUtil.sessionCookie(ConstantUtil.REFRESH_TOKEN_COOKIE, loginDto.getRefreshToken());
		refCookie.addCookie(response);
		
		loginService.insert(userService.getById(userDto.getId()), loginDto, IPUtil.getClientIp(request));
		
		return responseHandler.generateResponse(Boolean.TRUE);
	}
	
	@Tag(name = "인증 컨트롤러", description = "토큰을 이용한 인증 방식")
	@Operation(summary = "리프레시 토큰을 이용한 유효기간 연장", description = "acces 유효시간이 지나면 refresh를 이용하여 access 갱신한다.")
	@PostMapping(SecurityUtil.REFRESH_URL)
	public ResponseApi<Boolean> refresh(HttpServletResponse response) throws FileTransferException {
		AuthPayload payload = authUtil.getPayload();
		LoginDto loginDto = loginService.login(payload.getId(), payload.getRole());
		
		loginService.updateAccessToken(authUtil.getToken(), loginDto.getAccessToken());
		
		CookieUtil actCookie = CookieUtil.sessionCookie(ConstantUtil.ACCESS_TOKEN_COOKIE, loginDto.getAccessToken());
		actCookie.addCookie(response);
		
		return responseHandler.generateResponse(Boolean.TRUE);
	}
	
	@Tag(name = "인증 컨트롤러", description = "토큰을 이용한 인증 방식")
	@Operation(summary = "토큰 정보(ADMIN 전용)", description = "토큰으로 된 값이 뭐인지 불러온다.")
	@PostMapping(SecurityUtil.INFO_URL)
	public ResponseApi<AuthInfo> get(@RequestParam String token) throws FileTransferException {		
		return responseHandler.generateResponse(loginService.getTokenInfo(token));
	}
}
