package com.soobin.soobinzilla.dto.auth;

import com.soobin.soobinzilla.model.enums.Role;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AuthPayload {
	private Long	id;
	private Role	role;
}
