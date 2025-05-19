package com.soobin.soobinzilla.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.experimental.UtilityClass;

@UtilityClass
public class ObjectUtil {

	public static Boolean isEmpty(Object obj) {
		if(Objects.isNull(obj)) {
			return Boolean.TRUE;
		}
		if(obj instanceof String) {
			String str = (String) obj;
			return str.trim().isEmpty();
		} else if(obj instanceof List) {
			@SuppressWarnings("unchecked")
			List<Object> list = (List<Object>) obj;
			return list.isEmpty();
		}
		return Boolean.FALSE;
	}
	
	@SuppressWarnings("unchecked")
	public static Map<String, Object> toMap(Object obj) {
		ObjectMapper om = new ObjectMapper();
		return om.convertValue(obj, Map.class);
	}
	
	public static String toString(Object obj) {
		Map<String, Object> map = toMap(obj);
		ObjectMapper om = new ObjectMapper();
		try {
			return om.writeValueAsString(map);
		} catch (JsonProcessingException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	public static <T> T fromString(String str, Class<T> clazz) {
		try {
			ObjectMapper om = new ObjectMapper();
			return om.readValue(str, clazz);
		} catch (JsonProcessingException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	public static byte[] serializeToBytes(Serializable obj) {		
		try {
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			ObjectOutputStream oos = new ObjectOutputStream(baos);
			oos.writeObject(obj);
			oos.close();
			return baos.toByteArray();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return new byte[0];
	}
	
	public static <T extends Serializable> T toSerialize(byte[] serializedData) {
		try(ByteArrayInputStream bais = new ByteArrayInputStream(serializedData);
			ObjectInputStream ois = new ObjectInputStream(bais)) {			
			@SuppressWarnings("unchecked")
			T obj = (T) ois.readObject();
			return obj;
		} catch (IOException | ClassNotFoundException e) {
			e.printStackTrace();
		}
		return null;
	}	
	
}
