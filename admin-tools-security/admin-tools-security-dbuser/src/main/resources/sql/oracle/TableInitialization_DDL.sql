--------------------------------------------------------
--  DDL for Table AT_CLIENT
--------------------------------------------------------

  CREATE TABLE "AT_CLIENT" 
   (	
	"UUID" VARCHAR2(40 CHAR), 
	"CREATED" TIMESTAMP (6), 
	"CREATED_BY" VARCHAR2(255 CHAR), 
	"MODIFIED" TIMESTAMP (6), 
	"MODIFIED_BY" VARCHAR2(255 CHAR), 
	"JPAVERSION" NUMBER(10,0), 
	"ACTIVE" NUMBER(1,0), 
	"DESCRIPTION" VARCHAR2(255 CHAR), 
	"DISPLAY_NAME" VARCHAR2(255 CHAR), 
	"NAME" VARCHAR2(255 CHAR)
   );
--------------------------------------------------------
--  DDL for Table AT_ROLE
--------------------------------------------------------

  CREATE TABLE "AT_ROLE" 
   (	
	"UUID" VARCHAR2(40 CHAR), 
	"CREATED" TIMESTAMP (6), 
	"CREATED_BY" VARCHAR2(255 CHAR), 
	"MODIFIED" TIMESTAMP (6), 
	"MODIFIED_BY" VARCHAR2(255 CHAR), 
	"JPAVERSION" NUMBER(10,0), 
	"ACTIVE" NUMBER(1,0), 
	"DESCRIPTION" VARCHAR2(255 CHAR), 
	"DISPLAY_NAME" VARCHAR2(255 CHAR), 
	"NAME" VARCHAR2(255 CHAR)
   );
--------------------------------------------------------
--  DDL for Table AT_USER
--------------------------------------------------------

  CREATE TABLE "AT_USER" 
   (	
	"UUID" VARCHAR2(40 CHAR), 
	"CREATED" TIMESTAMP (6), 
	"CREATED_BY" VARCHAR2(255 CHAR), 
	"MODIFIED" TIMESTAMP (6), 
	"MODIFIED_BY" VARCHAR2(255 CHAR), 
	"JPAVERSION" NUMBER(10,0), 
	"ACC_EXP_SINCE" TIMESTAMP (6), 
	"ACC_LOCK_SINCE" TIMESTAMP (6), 
	"ACC_NON_EXP" NUMBER(1,0), 
	"ACC_NON_LOCK" NUMBER(1,0), 
	"CRED_EXP_SINCE" TIMESTAMP (6), 
	"CRED_NON_EXP" NUMBER(1,0), 
	"EMAIL" VARCHAR2(255 CHAR), 
	"FIRSTNAME" VARCHAR2(255 CHAR), 
	"LAST_LOGIN" TIMESTAMP (6), 
	"LAST_LOGIN_ATTEMPT" TIMESTAMP (6), 
	"LASTNAME" VARCHAR2(255 CHAR), 
	"LOCALE" VARCHAR2(5 CHAR), 
	"LOGIN_ATTEMPTS" NUMBER(10,0), 
	"PASSWORD" VARCHAR2(255 CHAR), 
	"PASSWORD_DATE" TIMESTAMP (6), 
	"PWD_LINK_CREATED" TIMESTAMP (6), 
	"PWD_LINK_HASH" VARCHAR2(255 CHAR), 
	"PHONE" VARCHAR2(255 CHAR), 
	"TIMEZONE" VARCHAR2(255 CHAR), 
	"USERNAME" VARCHAR2(255 CHAR)
   );
--------------------------------------------------------
--  DDL for Table AT_USER_CLIENTS
--------------------------------------------------------

  CREATE TABLE "AT_USER_CLIENTS" 
   (	
	"USER_UUID" VARCHAR2(40 CHAR), 
	"CLIENT_UUID" VARCHAR2(40 CHAR)
   );
--------------------------------------------------------
--  DDL for Table AT_USER_GROUP
--------------------------------------------------------

  CREATE TABLE "AT_USER_GROUP" 
   (	
	"UUID" VARCHAR2(40 CHAR), 
	"CREATED" TIMESTAMP (6), 
	"CREATED_BY" VARCHAR2(255 CHAR), 
	"MODIFIED" TIMESTAMP (6), 
	"MODIFIED_BY" VARCHAR2(255 CHAR), 
	"JPAVERSION" NUMBER(10,0), 
	"ACTIVE" NUMBER(1,0), 
	"DESCRIPTION" VARCHAR2(255 CHAR), 
	"DISPLAY_NAME" VARCHAR2(255 CHAR), 
	"NAME" VARCHAR2(255 CHAR)
   );
--------------------------------------------------------
--  DDL for Table AT_USER_USERGROUPS
--------------------------------------------------------

  CREATE TABLE "AT_USER_USERGROUPS" 
   (	
	"USER_UUID" VARCHAR2(40 CHAR), 
	"USERGROUP_UUID" VARCHAR2(40 CHAR)
   );
--------------------------------------------------------
--  DDL for Table AT_USERGROUPS_ROLES
--------------------------------------------------------

  CREATE TABLE "AT_USERGROUPS_ROLES" 
   (	
	"USERGROUP_UUID" VARCHAR2(40 CHAR), 
	"ROLE_UUID" VARCHAR2(40 CHAR)
   );
--------------------------------------------------------
--  DDL for Index IDX_AT_CLIENT_NAME
--------------------------------------------------------

  CREATE UNIQUE INDEX "IDX_AT_CLIENT_NAME" ON "AT_CLIENT" ("NAME");
--------------------------------------------------------
--  DDL for Index IDX_AT_CLIENT_UUID
--------------------------------------------------------

  CREATE UNIQUE INDEX "IDX_AT_CLIENT_UUID" ON "AT_CLIENT" ("UUID");
--------------------------------------------------------
--  DDL for Index IDX_AT_ROLE_NAME
--------------------------------------------------------

  CREATE UNIQUE INDEX "IDX_AT_ROLE_NAME" ON "AT_ROLE" ("NAME");
--------------------------------------------------------
--  DDL for Index IDX_AT_ROLE_UUID
--------------------------------------------------------

  CREATE UNIQUE INDEX "IDX_AT_ROLE_UUID" ON "AT_ROLE" ("UUID");
--------------------------------------------------------
--  DDL for Index IDX_AT_USER_USERNAME
--------------------------------------------------------

  CREATE UNIQUE INDEX "IDX_AT_USER_USERNAME" ON "AT_USER" ("USERNAME");
--------------------------------------------------------
--  DDL for Index IDX_AT_USER_UUID
--------------------------------------------------------

  CREATE UNIQUE INDEX "IDX_AT_USER_UUID" ON "AT_USER" ("UUID");
--------------------------------------------------------
--  DDL for Index IDX_AT_USER_CLIENTS
--------------------------------------------------------

  CREATE UNIQUE INDEX "IDX_AT_USER_CLIENTS" ON "AT_USER_CLIENTS" ("USER_UUID", "CLIENT_UUID");
--------------------------------------------------------
--  DDL for Index IDX_AT_USERGROUP_NAME
--------------------------------------------------------

  CREATE UNIQUE INDEX "IDX_AT_USERGROUP_NAME" ON "AT_USER_GROUP" ("NAME");
--------------------------------------------------------
--  DDL for Index IDX_AT_USERGROUP_UUID
--------------------------------------------------------

  CREATE UNIQUE INDEX "IDX_AT_USERGROUP_UUID" ON "AT_USER_GROUP" ("UUID");
--------------------------------------------------------
--  DDL for Index IDX_AT_USER_USERGROUP
--------------------------------------------------------

  CREATE UNIQUE INDEX "IDX_AT_USER_USERGROUP" ON "AT_USER_USERGROUPS" ("USER_UUID", "USERGROUP_UUID");
--------------------------------------------------------
--  DDL for Index IDX_AT_USERGROUP_ROLES
--------------------------------------------------------

  CREATE UNIQUE INDEX "IDX_AT_USERGROUP_ROLES" ON "AT_USERGROUPS_ROLES" ("USERGROUP_UUID", "ROLE_UUID");
--------------------------------------------------------
--  Constraints for Table AT_CLIENT
--------------------------------------------------------

  ALTER TABLE "AT_CLIENT" MODIFY ("UUID" NOT NULL ENABLE);
  ALTER TABLE "AT_CLIENT" MODIFY ("CREATED" NOT NULL ENABLE);
  ALTER TABLE "AT_CLIENT" MODIFY ("JPAVERSION" NOT NULL ENABLE);
  ALTER TABLE "AT_CLIENT" MODIFY ("ACTIVE" NOT NULL ENABLE);
  ALTER TABLE "AT_CLIENT" MODIFY ("NAME" NOT NULL ENABLE);
  ALTER TABLE "AT_CLIENT" ADD PRIMARY KEY ("UUID") USING INDEX  ENABLE;
  ALTER TABLE "AT_CLIENT" ADD CONSTRAINT "IDX_AT_CLIENT_NAME" UNIQUE ("NAME") USING INDEX ENABLE;
--------------------------------------------------------
--  Constraints for Table AT_ROLE
--------------------------------------------------------

  ALTER TABLE "AT_ROLE" MODIFY ("UUID" NOT NULL ENABLE);
  ALTER TABLE "AT_ROLE" MODIFY ("CREATED" NOT NULL ENABLE);
  ALTER TABLE "AT_ROLE" MODIFY ("JPAVERSION" NOT NULL ENABLE);
  ALTER TABLE "AT_ROLE" MODIFY ("ACTIVE" NOT NULL ENABLE);
  ALTER TABLE "AT_ROLE" MODIFY ("NAME" NOT NULL ENABLE);
  ALTER TABLE "AT_ROLE" ADD PRIMARY KEY ("UUID") USING INDEX ENABLE;
  ALTER TABLE "AT_ROLE" ADD CONSTRAINT "IDX_AT_ROLE_NAME" UNIQUE ("NAME") USING INDEX ENABLE;
--------------------------------------------------------
--  Constraints for Table AT_USER
--------------------------------------------------------

  ALTER TABLE "AT_USER" MODIFY ("PASSWORD" NOT NULL ENABLE);
  ALTER TABLE "AT_USER" MODIFY ("TIMEZONE" NOT NULL ENABLE);
  ALTER TABLE "AT_USER" MODIFY ("USERNAME" NOT NULL ENABLE);
  ALTER TABLE "AT_USER" ADD PRIMARY KEY ("UUID") USING INDEX ENABLE;
  ALTER TABLE "AT_USER" ADD CONSTRAINT "IDX_AT_USER_USERNAME" UNIQUE ("USERNAME") USING INDEX ENABLE;
  ALTER TABLE "AT_USER" MODIFY ("UUID" NOT NULL ENABLE);
  ALTER TABLE "AT_USER" MODIFY ("CREATED" NOT NULL ENABLE);
  ALTER TABLE "AT_USER" MODIFY ("JPAVERSION" NOT NULL ENABLE);
  ALTER TABLE "AT_USER" MODIFY ("ACC_NON_EXP" NOT NULL ENABLE);
  ALTER TABLE "AT_USER" MODIFY ("ACC_NON_LOCK" NOT NULL ENABLE);
  ALTER TABLE "AT_USER" MODIFY ("CRED_NON_EXP" NOT NULL ENABLE);
  ALTER TABLE "AT_USER" MODIFY ("LOCALE" NOT NULL ENABLE);
  ALTER TABLE "AT_USER" MODIFY ("LOGIN_ATTEMPTS" NOT NULL ENABLE);
--------------------------------------------------------
--  Constraints for Table AT_USER_CLIENTS
--------------------------------------------------------

  ALTER TABLE "AT_USER_CLIENTS" MODIFY ("USER_UUID" NOT NULL ENABLE);
  ALTER TABLE "AT_USER_CLIENTS" MODIFY ("CLIENT_UUID" NOT NULL ENABLE);
  ALTER TABLE "AT_USER_CLIENTS" ADD PRIMARY KEY ("USER_UUID", "CLIENT_UUID") USING INDEX ENABLE;
--------------------------------------------------------
--  Constraints for Table AT_USER_GROUP
--------------------------------------------------------

  ALTER TABLE "AT_USER_GROUP" MODIFY ("UUID" NOT NULL ENABLE);
  ALTER TABLE "AT_USER_GROUP" MODIFY ("CREATED" NOT NULL ENABLE);
  ALTER TABLE "AT_USER_GROUP" MODIFY ("JPAVERSION" NOT NULL ENABLE);
  ALTER TABLE "AT_USER_GROUP" MODIFY ("ACTIVE" NOT NULL ENABLE);
  ALTER TABLE "AT_USER_GROUP" MODIFY ("NAME" NOT NULL ENABLE);
  ALTER TABLE "AT_USER_GROUP" ADD PRIMARY KEY ("UUID") USING INDEX ENABLE;
  ALTER TABLE "AT_USER_GROUP" ADD CONSTRAINT "IDX_AT_USERGROUP_NAME" UNIQUE ("NAME") USING INDEX ENABLE;
--------------------------------------------------------
--  Constraints for Table AT_USER_USERGROUPS
--------------------------------------------------------

  ALTER TABLE "AT_USER_USERGROUPS" MODIFY ("USER_UUID" NOT NULL ENABLE);
  ALTER TABLE "AT_USER_USERGROUPS" MODIFY ("USERGROUP_UUID" NOT NULL ENABLE);
  ALTER TABLE "AT_USER_USERGROUPS" ADD PRIMARY KEY ("USER_UUID", "USERGROUP_UUID") USING INDEX ENABLE;
--------------------------------------------------------
--  Constraints for Table AT_USERGROUPS_ROLES
--------------------------------------------------------

  ALTER TABLE "AT_USERGROUPS_ROLES" MODIFY ("USERGROUP_UUID" NOT NULL ENABLE);
  ALTER TABLE "AT_USERGROUPS_ROLES" MODIFY ("ROLE_UUID" NOT NULL ENABLE);
  ALTER TABLE "AT_USERGROUPS_ROLES" ADD PRIMARY KEY ("USERGROUP_UUID", "ROLE_UUID") USING INDEX ENABLE;
--------------------------------------------------------
--  Ref Constraints for Table AT_USER_CLIENTS
--------------------------------------------------------

  ALTER TABLE "AT_USER_CLIENTS" ADD CONSTRAINT "FK_AT_USER_CLIENTS_CLIENT" FOREIGN KEY ("CLIENT_UUID")
	  REFERENCES "AT_CLIENT" ("UUID") ENABLE;
  ALTER TABLE "AT_USER_CLIENTS" ADD CONSTRAINT "FK_AT_USER_CLIENTS_USER" FOREIGN KEY ("USER_UUID")
	  REFERENCES "AT_USER" ("UUID") ENABLE;
--------------------------------------------------------
--  Ref Constraints for Table AT_USER_USERGROUPS
--------------------------------------------------------

  ALTER TABLE "AT_USER_USERGROUPS" ADD CONSTRAINT "FK_AT_USER_USERGROUPS_UG" FOREIGN KEY ("USERGROUP_UUID")
	  REFERENCES "AT_USER_GROUP" ("UUID") ENABLE;
  ALTER TABLE "AT_USER_USERGROUPS" ADD CONSTRAINT "FK_AT_USER_USERGROUPS_USER" FOREIGN KEY ("USER_UUID")
	  REFERENCES "AT_USER" ("UUID") ENABLE;
--------------------------------------------------------
--  Ref Constraints for Table AT_USERGROUPS_ROLES
--------------------------------------------------------

  ALTER TABLE "AT_USERGROUPS_ROLES" ADD CONSTRAINT "FK_AT_USERGROUPS_ROLES_ROLE" FOREIGN KEY ("ROLE_UUID")
	  REFERENCES "AT_ROLE" ("UUID") ENABLE;
  ALTER TABLE "AT_USERGROUPS_ROLES" ADD CONSTRAINT "FK_AT_USERGROUPS_ROLES_UG" FOREIGN KEY ("USERGROUP_UUID")
	  REFERENCES "AT_USER_GROUP" ("UUID") ENABLE;
