package com.soobin.soobinzilla.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class UsersDeleteDto {
	@Schema(description = "유저 아이디")
	private	Long	id;
}
