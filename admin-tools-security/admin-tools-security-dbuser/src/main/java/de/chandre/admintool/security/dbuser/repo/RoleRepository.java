package de.chandre.admintool.security.dbuser.repo;

import java.util.List;
import java.util.Set;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import de.chandre.admintool.security.dbuser.domain.ATRole;

/**
 * 
 * @author André
 * @since 1.1.7
 */
@Repository
public interface RoleRepository extends JpaRepository<ATRole, String> {
	
	ATRole findByName(String name);
	
	@Query("SELECT r.name FROM ATRole r")
	List<String> findAllRoleNames();
	
	List<ATRole> findByNameIn(Set<String> ids);
	
	List<ATRole> findByIdIn(Set<String> ids);
	
	void deleteByName(String name);
}
