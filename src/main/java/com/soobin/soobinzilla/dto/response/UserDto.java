package com.soobin.soobinzilla.dto.response;

import java.time.LocalDateTime;

import com.soobin.soobinzilla.model.enums.Role;

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
public class UserDto {
	private	Long	id;
	private String 	userId;
	private String 	password;
	private String	name;
	private Long	departmentId;
	private Role	role;
	private LocalDateTime createAt;
}
