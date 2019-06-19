package de.chandre.admintool;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;

import org.springframework.stereotype.Component;

import de.chandre.admintool.core.AdminToolConfig;

@Component
public class ExampleMXBean implements AdminToolConfig {
	
	private Double myDouble = Double.valueOf(123.456d);
	private double myPrimitiveDouble =  654.321d;
	
	public Date getDate() {
		return new Date();
	}
	
	public LocalDate getLocalDate() {
		return LocalDate.now();
	}
	
	public LocalDateTime getLocalDateTime() {
		return LocalDateTime.now();
	}
	
	public Double getDouble() {
		return myDouble;
	}
	
	public void setDouble(Double doubleValue) {
		this.myDouble = doubleValue;
	}
	
	public double getPrimitiveDouble() {
		return this.myPrimitiveDouble;
	}
	
	public void setPrimitiveDouble(double doubleValue) {
		this.myPrimitiveDouble = doubleValue;
	}

	@Override
	public void printConfig() {
		
	}

	@Override
	public boolean isEnabled() {
		return true;
	}
}
