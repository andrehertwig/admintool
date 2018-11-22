package de.chandre.admintool.security.dbuser.repo;

import java.util.Collection;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import de.chandre.admintool.security.dbuser.domain.ATUser;
import de.chandre.admintool.security.dbuser.domain.ATUserGroup;

/**
 * 
 * @author André
 * @since 1.1.7
 */
@Repository
public interface UserRepository extends JpaRepository<ATUser, String> {
	
	ATUser findByUsername(String username);
	
	ATUser findByPasswordLinkHash(String passwordLinkHash);
	
	void deleteByUsername(String username);
	
	int countUsersByUserGroupsIn(Collection<ATUserGroup> userGroups);
}
