package de.chandre.admintool.security.dbuser.repo;

import java.util.List;
import java.util.Set;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import de.chandre.admintool.security.dbuser.domain.ATClient;

/**
 * 
 * @author André
 * @since 1.1.7
 */
@Repository
public interface ClientRepository extends JpaRepository<ATClient, String> {
	
	ATClient findByName(String name);
	
	@Query("SELECT c.name FROM ATClient c")
	List<String> findAllClientNames();

	List<ATClient> findByNameIn(Set<String> clientNames);
	
	void deleteByName(String name);
}
