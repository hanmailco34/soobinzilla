package com.soobin.soobinzilla.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;

@Configuration
public class SwaggerConfig {
	
	@Value("${app.api.prefix}") 
	private String apiPrefix;

	@Bean
	Docket api() {
		return new Docket(DocumentationType.SWAGGER_2)
				.apiInfo(getApiInfo())
				.select()
				.apis(RequestHandlerSelectors.basePackage("com.soobin.soobinzilla.controller"))
				.paths(PathSelectors.any())
				.build();
	}
	
	private ApiInfo getApiInfo() {
		return new ApiInfoBuilder()
				.title("코난 질라")
				.description("코난 질라질라")
				.version(apiPrefix)
				.build();
	}
}
