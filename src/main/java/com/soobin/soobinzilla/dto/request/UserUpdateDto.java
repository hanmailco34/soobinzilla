package com.soobin.soobinzilla.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class UserUpdateDto {
	@Schema(description = "유저 비밀번호 숫자,알파벳문자,특수문자 적어도 하나 포함")
	private String 	password;
	@Schema(description = "유저 이름")
	private String	name;
}
