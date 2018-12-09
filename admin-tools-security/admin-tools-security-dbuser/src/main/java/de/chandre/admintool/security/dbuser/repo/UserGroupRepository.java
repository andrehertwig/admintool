package de.chandre.admintool.security.dbuser.repo;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import de.chandre.admintool.security.dbuser.domain.ATRole;
import de.chandre.admintool.security.dbuser.domain.ATUserGroup;

/**
 * 
 * @author André
 * @since 1.2.0
 */
@Repository
public interface UserGroupRepository extends JpaRepository<ATUserGroup, String> {
	
	ATUserGroup findByName(String name);
	
	@Query("SELECT ug.name FROM ATUserGroup ug")
	List<String> findAllATUserGroupNames();
	
	List<ATUserGroup> findByNameIn(Collection<String> names);
	
	List<ATUserGroup> findByIdIn(Set<String> ids);
	
	void deleteByName(String name);

	int countUserGroupsByRolesIn(List<ATRole> asList);
	
	List<ATUserGroup> findByRolesIn(List<ATRole> asList);
}
