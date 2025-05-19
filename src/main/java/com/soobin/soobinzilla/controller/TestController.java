package com.soobin.soobinzilla.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import springfox.documentation.annotations.ApiIgnore;

@Getter
@Setter
@ToString 
class TestVO {
	private String id;
}

@RestController
@RequestMapping("/v1/test")
@RequiredArgsConstructor
@ApiIgnore
public class TestController {
	
	@GetMapping("/test2/{id}")
	public String test2(@PathVariable String id) {
		return id;
	}
	
	@PostMapping("/test3/{id}")
	public String test3(@PathVariable String id, @RequestBody TestVO testvo) {
		return id + testvo.getId();
	}
	
	@PostMapping("/test4/{id}")
	public String test4(@PathVariable String id, TestVO testvo) {
		return id + testvo.getId();
	}	
}
