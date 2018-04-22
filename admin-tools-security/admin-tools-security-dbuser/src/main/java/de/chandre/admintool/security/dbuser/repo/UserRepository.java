package de.chandre.admintool.security.dbuser.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import de.chandre.admintool.security.dbuser.domain.ATUser;

/**
 * 
 * @author André
 * @since 1.1.7
 */
@Repository
public interface UserRepository extends JpaRepository<ATUser, String> {
	
	ATUser findByUsername(String username);
	
	void deleteByUsername(String username);
}
