package com.soobin.soobinzilla.util;

import java.util.Random;

import lombok.experimental.UtilityClass;

@UtilityClass
public class RandomUtil {
	
	Random random = new Random();

	public static int getRandomIndex(int total, int length) {
		if(total < length) return 0;
		
		return random.nextInt(total - length + 1);
	}
}
