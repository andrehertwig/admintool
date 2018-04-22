package de.chandre.admintool.security.dbuser.domain;

import java.util.Set;

import org.springframework.security.core.GrantedAuthority;

public interface Role extends GrantedAuthority, AccessRelation {

	Set<ATUserGroup> getUserGroups();
}
