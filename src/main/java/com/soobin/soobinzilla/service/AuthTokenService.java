package com.soobin.soobinzilla.service;

import java.util.Optional;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.soobin.soobinzilla.dto.auth.AuthHeader;
import com.soobin.soobinzilla.dto.auth.AuthInfo;
import com.soobin.soobinzilla.dto.auth.AuthPayload;
import com.soobin.soobinzilla.dto.auth.AuthTokenParts;
import com.soobin.soobinzilla.dto.response.LoginDto;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.TokenErrorCode;
import com.soobin.soobinzilla.mapper.AuthTokenMapper;
import com.soobin.soobinzilla.model.AuthToken;
import com.soobin.soobinzilla.model.User;
import com.soobin.soobinzilla.model.enums.Role;
import com.soobin.soobinzilla.repository.auth.AuthTokenRepository;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.CryptoUtil;
import com.soobin.soobinzilla.util.ObjectUtil;
import com.soobin.soobinzilla.util.RandomUtil;
import com.soobin.soobinzilla.util.TimeUtil;
import com.soobin.soobinzilla.util.ValidUtil;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class AuthTokenService {
	
	private final AuthTokenRepository authTokenRepository;
	
	private final AuthTokenMapper authTokenMapper;
	
	private final CryptoUtil cryptoUtil;	
	
	@Value("${app.token.access}") 
	private Long accessTime;
	
	@Value("${app.token.refresh}") 
	private Long refreshTime;
	
	public LoginDto login(Long id, Role role) {
		String accessToken = getToken(id, role, ConstantUtil.ACCESS_TOKEN);
		String refreshToken = getToken(id, role, ConstantUtil.REFRESH_TOKEN);
		
		return LoginDto.builder()
				.accessToken(accessToken)
				.refreshToken(refreshToken)
				.build();
	}
	
	public void logout(Long id) {
		authTokenRepository.deactivateTokensByUser(id);
	}
	
	public void insert(User user, LoginDto loginDto, String ip) {
		AuthToken loginToken = authTokenMapper.toLoginToken(user, loginDto.getAccessToken(), loginDto.getRefreshToken(), ip);
		authTokenRepository.save(loginToken);
	}
	
	private AuthToken getActiveByRefreshToken(String refreshToken) {
		Optional<AuthToken> optionalAuthToken = authTokenRepository.findByRefreshTokenAndIsActive(refreshToken, Boolean.TRUE);		
		return optionalAuthToken.orElse(null);
	}
	
	private AuthToken getActiveByAccessToken(String accessToken) {
		Optional<AuthToken> optionalAuthToken = authTokenRepository.findByAccessTokenAndIsActive(accessToken, Boolean.TRUE);		
		return optionalAuthToken.orElse(null);
	}
	
	public AuthToken checkToken(String token, String type) throws FileTransferException {
		AuthToken authToken = null;
		if(ConstantUtil.REFRESH_TOKEN.equals(type)) {
			authToken = getActiveByRefreshToken(token);
		} else if(ConstantUtil.ACCESS_TOKEN.equals(type)) {
			authToken = getActiveByAccessToken(token);
		}
		
		if(authToken == null) throw new FileTransferException(TokenErrorCode.NOT_ACTIVE, token);
		
		return authToken;
	}
	
	public void updateAccessToken(String refreshToken, String accessToken) throws FileTransferException {
		AuthToken authToken = checkToken(refreshToken, ConstantUtil.REFRESH_TOKEN);
		
		authToken.updateAccessToken(accessToken);
		authTokenRepository.save(authToken);
	}
	
	private String getToken(Long id, Role role, String type) {
		Integer index = RandomUtil.getRandomIndex(cryptoUtil.getKeyBook().length(), cryptoUtil.getKeyBookLength());
		
		String header = getTokenHeader(type, index);
		String payload = getTokenPayLoad(id, role, index);
		String signatrue = getTokenSignature(header, payload);
		
		return header + "." + payload + "." + signatrue;
	}
	
	private String getTokenHeader(String type, Integer index) {
		Long time = (ConstantUtil.REFRESH_TOKEN.equals(type)) ? refreshTime : accessTime;
		
		
		AuthHeader header = AuthHeader.builder()
				.expireTime(TimeUtil.getNowMillisecond() + time)
				.tokenType(type)
				.keyIndex(index)
				.build();
		
		String headerStr = ObjectUtil.toString(header);
		
		return cryptoUtil.encrypt(headerStr);
	}
	
	private String getTokenPayLoad(Long id, Role role, Integer index) {
		AuthPayload payload = AuthPayload.builder()
				.id(id)
				.role(role)
				.build();
		
		String payloadStr = ObjectUtil.toString(payload);
		
		return cryptoUtil.encrypt(payloadStr, index);
	}
	
	private String getTokenSignature(String header, String payload) {
		return cryptoUtil.sha512(header + payload);
	}
	
	public AuthInfo getTokenInfo(String token) throws FileTransferException {		
		AuthTokenParts tokenParts = getTokenParts(token);
		
		// 시그니처 체크
		if(Boolean.FALSE.equals(checkSignature(tokenParts.getHeader(), tokenParts.getPayload(), tokenParts.getSignature()))) throw new FileTransferException(TokenErrorCode.NOT_SIGNATURE, token);
		
		AuthHeader header = getHeader(tokenParts.getHeader());
		
		Integer keyIndex = header.getKeyIndex();
		// keyIndex check
		if(Boolean.FALSE.equals(checkKeyIndex(keyIndex))) throw new FileTransferException(TokenErrorCode.KEY_LENGTH_OVER, token);
		
		AuthPayload payload = getPayload(tokenParts.getPayload(), keyIndex);
		
		return AuthInfo.builder()
				.authHeader(header)
				.authPayload(payload)
				.build();
	}
	
	public AuthPayload checkTokenAndGetPayload(String token) throws FileTransferException {
		AuthInfo authInfo = getTokenInfo(token);
		
		// 유효시간 체크
		if(Boolean.TRUE.equals(TimeUtil.isExpire(authInfo.getAuthHeader().getExpireTime()))) throw new FileTransferException(TokenErrorCode.EXPIRE_TIME, token);
		
		return authInfo.getAuthPayload();
	}
	
	private AuthTokenParts getTokenParts(String token) throws FileTransferException {
		String[] tokenArray = token.split("\\.");
		
		if(tokenArray.length != 3) throw new FileTransferException(TokenErrorCode.NOT_FORMAT, token);
		
		return AuthTokenParts.builder()
				.header(tokenArray[0])
				.payload(tokenArray[1])
				.signature(tokenArray[2])
				.build();
	}
	
	private Boolean checkSignature(String header, String payload, String signatrue) {
		return getTokenSignature(header, payload).equals(signatrue);
	}
	
	private AuthHeader getHeader(String headerStr) {
		String decryptHeader = cryptoUtil.decrypt(headerStr);
		return ObjectUtil.fromString(decryptHeader, AuthHeader.class);
	}
	
	private Boolean checkKeyIndex(Integer keyIndex) {
		if(keyIndex < 0) return false;
		return ValidUtil.isLengthOver(cryptoUtil.getKeyBook(), keyIndex + cryptoUtil.getKeyBookLength());
	}
	
	private AuthPayload getPayload(String payloadStr, Integer keyIndex) {
		String decrypPayload = cryptoUtil.decrypt(payloadStr, keyIndex);
		return ObjectUtil.fromString(decrypPayload, AuthPayload.class);
	}
}
