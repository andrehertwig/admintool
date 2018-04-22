package de.chandre.admintool.core.utils;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Locale;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.util.ReflectionUtils;

/**
 * 
 * @author André
 * @since 1.1.7
 *
 */
public class ReflectUtils
{
	private static final Log LOGGER = LogFactory.getLog(ReflectUtils.class);
	
	private ReflectUtils(){};
	
	private static final List<String> PREFIXES = Arrays.asList("is", "has", "have", "shall", "should", "can", "could"); 
	
	
	/**
	 * will copy fields with same declaration name<br>
	 * 
	 * @param clazz the starting/root class of input (maybe input class is wrapped by spring, hibernate or another framework so you should define the root class)
	 * @param input the input object
	 * @param output the object to fill / override
	 * @param copyCollections if collections should be copied 
	 * 		(if true you may get a <code>org.hibernate.HibernateException: Found shared references to a collection</code> on entities)
	 * @param ignoreFields list of field names to ignore
	 * @param ignoreNonExisiting do not throw NPE if setter on output doesn't exists
	 */
	public static <O extends Object> O copyFields(Class<?> clazz, Object input, O output, boolean copyCollections, 
			Collection<String> ignoreFields, boolean ignoreNonExisiting) {
		
		if (null == clazz) {
			clazz = input.getClass();
		}
		Field[] fields = clazz.getDeclaredFields();
		for (Field field : fields) {
			/*
			 *  do not copy collections because of possible
			 *  org.hibernate.HibernateException: Found shared references to a collection
			 */
			if (ReflectUtils.isNotStatic(field)) {
				if (null != ignoreFields && ignoreFields.contains(field.getName())) {
					if(LOGGER.isTraceEnabled()) {
						LOGGER.trace(String.format("ignoring field: %s", field.getName()));
					}
					continue;
				}
				if (Collection.class.isAssignableFrom(field.getType()) && !copyCollections) {
					if(LOGGER.isTraceEnabled()) {
						LOGGER.trace(String.format("skipping collection field: %s", field.getName()));
					}
					continue;
				}
				if(LOGGER.isTraceEnabled()) {
					LOGGER.trace(String.format("copying field: %s, with value: %s", field.getName(), ReflectUtils.invokeGetter(input, field)));
				}
				ReflectUtils.copy(input, output, field.getName(), ignoreNonExisiting);
			}
		}
		Class<?> superClazz = clazz.getSuperclass();
		if (null != superClazz && !superClazz.isAssignableFrom(Object.class)) {
			output = copyFields(superClazz, input, output, copyCollections, ignoreFields, ignoreNonExisiting);
		}
		return output;
	}

	
	/**
	 * creates getter out of field name
	 * 
	 * @param fieldName
	 * @return
	 */
	public static String toGetter(final String fieldName) {
		return toGetter(fieldName, "get");
	}
	
	public static String toGetter(final String fieldName, String prefix) {
		return prefix + fieldName.substring(0, 1).toUpperCase(Locale.ENGLISH) + fieldName.substring(1, fieldName.length());
	}
	
	/**
	 * creates setter out of field name
	 * 
	 * @param fieldName
	 * @return
	 */
	public static String toSetter(final String fieldName) {
		return "set" + fieldName.substring(0, 1).toUpperCase(Locale.ENGLISH) + fieldName.substring(1, fieldName.length());
	}
	
	/**
	 * checks if the attribute is static
	 * @param field
	 * @return
	 */
	public static boolean isStatic(Field field) {
		return Modifier.isStatic(field.getModifiers());
	}
	
	/**
	 * checks if the attribute is NOT static
	 * @param field
	 * @return
	 */
	public static boolean isNotStatic(Field field) {
		return !isStatic(field);
	}
	
	/**
	 * invokes the getter on the filed
	 * 
	 * @param object
	 * @param field
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static <S extends Serializable> S invokeGetter(Object object, Field field)
	{
		Method m = ReflectionUtils.findMethod(object.getClass(), ReflectUtils.toGetter(field.getName()));
		if (null == m) {
			for (String prefix : PREFIXES) {
				 m = ReflectionUtils.findMethod(object.getClass(), ReflectUtils.toGetter(field.getName(), prefix));
				 if (null != m) {
					 break;
				 }
			}
		}
		return (S) ReflectionUtils.invokeMethod(m, object);
	}
	
	/**
	 * invokes the getter on the filed
	 * 
	 * @param object
	 * @param field
	 * @return
	 */
	public static <S extends Serializable> S invokeGetter(Object object, String fieldName)
	{
		Field field = ReflectionUtils.findField(object.getClass(), fieldName);
		return invokeGetter(object, field);
	}
	
	/**
	 * invokes the setter on the filed
	 * 
	 * @param object
	 * @param field
	 * @return
	 */
	public static void invokeSetter(Object object, Field field, Object value, boolean ignoreNonExisting)
	{
		if(null == field && ignoreNonExisting) {
			return;
		}
		ReflectionUtils.invokeMethod(ReflectionUtils.findMethod(object.getClass(), ReflectUtils.toSetter(field.getName()), field.getType()), object, value);
	}
	
	/**
	 * invokes the setter on the filed
	 * 
	 * @param object
	 * @param field
	 * @return
	 */
	public static void invokeSetter(Object object, String fieldName, Object value, boolean ignoreNonExisting)
	{
		Field field = ReflectionUtils.findField(object.getClass(), fieldName);
		if(null == field) {
			if (ignoreNonExisting) {
				return;
			} else {
				throw new NullPointerException("no field with name " + fieldName + " found on output object. ignoreNonExisting was " + ignoreNonExisting);
			}
		}
		invokeSetter(object, field, value, ignoreNonExisting);
	}
	
	/**
	 * copys a value from one to another object with same field name!
	 * 
	 * @param object
	 * @param clone
	 * @param fieldName
	 */
	public static void copy(Object object, Object clone, String fieldName, boolean ignoreNonExisting)
	{
		invokeSetter(clone, fieldName, invokeGetter(object, fieldName), ignoreNonExisting);
	}
	
	/**
	 * copys a value from one to another object !
	 * 
	 * @param object
	 * @param field
	 * @param clone
	 * @param cloneField
	 */
	public static void copy(Object object, Field field, Object clone, Field cloneField, boolean ignoreNonExisting)
	{
		invokeSetter(clone, cloneField, invokeGetter(object, field), ignoreNonExisting);
	}
}
