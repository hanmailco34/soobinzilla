package com.soobin.soobinzilla.dto.request;

import com.soobin.soobinzilla.model.enums.Role;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class UsersUpdateDto {
	@Schema(description = "유저 아이디")
	private	Long	id;
	@Schema(description = "유저 역할(MEMBER, ADMIN)")
	private Role	role;
	@Schema(description = "유저 비밀번호 숫자,알파벳문자,특수문자 적어도 하나 포함")
	private String 	password;
	@Schema(description = "유저 이름")
	private String	name;
	@Schema(description = "부서 아이디(어드민으로 삽입시 생략)")
	private Long	departmentId;
}
