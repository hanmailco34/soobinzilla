package com.soobin.soobinzilla.dto.response;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConnectionDto {
	@Schema(description = "연결 ID", example = "1")
	private Long	id;
	@Schema(description = "호스트 이름(IP)", example = "127.0.0.1")
	private String	host;
	@Schema(description = "포트 번호", example = "8080")
	private Integer	port;
	@Schema(description = "사용자 이름", example = "user1")
	private String	username;
	@Schema(description = "사용자 비밀번호", example = "password123")
	private String	password;
	@Schema(description = "보안 연결 여부", example = "false")
	private Boolean	isSecure;
	@Schema(description = "연결 설정", implementation = ConnectionConfigDto.class)
	private ConnectionConfigDto connectionConfig;
}
