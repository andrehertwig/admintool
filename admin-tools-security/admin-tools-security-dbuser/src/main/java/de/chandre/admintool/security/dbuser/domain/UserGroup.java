package de.chandre.admintool.security.dbuser.domain;

import java.util.Set;
import java.util.stream.Stream;

public interface UserGroup extends AccessRelation {

	Set<ATUser> getUsers();

	Stream<ATRole> getActiveRoles();

	Set<ATRole> getRoles();

}
