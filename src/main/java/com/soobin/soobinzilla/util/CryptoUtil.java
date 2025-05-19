package com.soobin.soobinzilla.util;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.security.MessageDigest;
import java.security.SecureRandom;
import java.util.Arrays;

import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class CryptoUtil {
	
	@Value("${app.aes.key}")
	private String aesKey;
	
	@Value("${app.aes.key-book}")
	private String aesKeyBook;
	
	@Value("${app.aes.key-book-length}")
	private Integer aesKeyBookLength;
	
	private static final int IV_LENGTH = 12;
    private static final int TAG_LENGTH = 128;
	
	public String sha512(String param) {
		return Boolean.TRUE.equals(ObjectUtil.isEmpty(param)) ? null : shaEncrypt(param, "SHA-512");
	}
	
	private String shaEncrypt(String param, String algorithm) {
		String toReturn = null;
		try {
			MessageDigest messageDigest = MessageDigest.getInstance(algorithm);
			messageDigest.reset();
			messageDigest.update(param.getBytes(StandardCharsets.UTF_8));
			toReturn = String.format("%0128x", new BigInteger(1, messageDigest.digest()));
		} catch (Exception e) {
		    e.printStackTrace();
		}
		return toReturn;
	}
	
	public Integer getKeyBookLength() {
		return this.aesKeyBookLength;
	}
	
	public String getKeyBook() {
		return this.aesKeyBook;
	}
	
	public String encrypt(String text) {
		return encrypt(text.getBytes());
	}
	
	public String encrypt(String text, Integer keyIndex) {
		Key skey = makeAESKey(keyIndex);
		return encrypt(skey, text.getBytes());
	}
	
	public String encrypt(byte[] text) {
		Key skey = makeAESKey(aesKey);
		return encrypt(skey, text);
	}
	
	private String encrypt(Key skey, byte[] text) {
		try {
			byte[] iv = setRandomIV();
			byte[] enc = aesToByte(text, skey, Cipher.ENCRYPT_MODE, iv);
			byte[] combined = new byte[iv.length + enc.length];
            System.arraycopy(iv, 0, combined, 0, iv.length);
            System.arraycopy(enc, 0, combined, iv.length, enc.length);
			return byte2hex(combined);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}
	
	public String decrypt(String text) {
		Key skey = makeAESKey(aesKey);
		return decryptText(skey, text);
	}
	
	public String decrypt(String text, Integer keyIndex) {
		Key skey = makeAESKey(keyIndex);
		return decryptText(skey, text);
	}
	
	private String decryptText(Key skey, String text) {
		try {
			byte[] dec = decrypt(skey, text);
			return new String(dec);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}
	
	private byte[] decrypt(Key skey, String text) {		
		try {			
			byte[] combined = hexToByteArray(text);
			byte[] iv = Arrays.copyOfRange(combined, 0, IV_LENGTH);
			byte[] enc = Arrays.copyOfRange(combined, IV_LENGTH, combined.length);
			return aesToByte(enc, skey, Cipher.DECRYPT_MODE, iv);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return new byte[0];
	}
	
	private Key makeAESKey(String sKey) {
		final byte[] key = sKey.getBytes();
		return new SecretKeySpec(key, "AES");
	}
	
	private Key makeAESKey(Integer keyIndex) {
		String sKey = this.aesKeyBook.substring(keyIndex, keyIndex + aesKeyBookLength);
		return makeAESKey(sKey);
	}
	
	private byte[] aesToByte(byte[] src, Key skey, Integer mode, byte[] iv) throws Exception {		
		Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
		
		GCMParameterSpec spec = new GCMParameterSpec(TAG_LENGTH, iv);
		
		cipher.init(mode, skey, spec);
		return cipher.doFinal(src);
	}
	
	private byte[] setRandomIV() {
		byte[] bytes = new byte[IV_LENGTH];
        SecureRandom random = new SecureRandom();
        random.nextBytes(bytes);
        return bytes;
	}
	
	private String byte2hex(byte[] b) {
		StringBuilder builder = new StringBuilder();
		for (byte value : b) {
			String stmp = Integer.toHexString(value & 0XFF);
			if (stmp.length() == 1) {
				builder.append("0");
			}
			builder.append(stmp);
		}
		return builder.toString();
	}
	
	private byte[] hexToByteArray(String hex) {
		if (hex == null || hex.length() == 0) {
			return new byte[0];
		}

		byte[] ba = new byte[hex.length() / 2];
		for (int i = 0; i < ba.length; i++) {
			ba[i] = (byte) Integer.parseInt(hex.substring(2 * i, 2 * i + 2), 16);
		}
		return ba;
	}

}
