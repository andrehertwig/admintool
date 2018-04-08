package de.chandre.admintool;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;

import org.springframework.stereotype.Component;

import de.chandre.admintool.core.AdminToolConfig;

@Component
public class ExampleMXBean implements AdminToolConfig {
	
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
		return Double.valueOf(123.456d);
	}
	
	public double getPrimitiveDouble() {
		return 654.321d;
	}

	@Override
	public void printConfig() {
		
	}

	@Override
	public boolean isEnabled() {
		return true;
	}
}
