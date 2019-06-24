CREATE TABLE "at_client"
(
   uuid varchar(40) PRIMARY KEY NOT NULL,
   created timestamp NOT NULL,
   created_by varchar(255),
   modified timestamp,
   modified_by varchar(255),
   jpaversion int NOT NULL,
   active bool NOT NULL,
   description varchar(255),
   display_name varchar(255),
   name varchar(255) NOT NULL
)
;
CREATE TABLE "at_role"
(
   uuid varchar(40) PRIMARY KEY NOT NULL,
   created timestamp NOT NULL,
   created_by varchar(255),
   modified timestamp,
   modified_by varchar(255),
   jpaversion int NOT NULL,
   active bool NOT NULL,
   description varchar(255),
   display_name varchar(255),
   name varchar(255) NOT NULL
)
;
CREATE TABLE "at_user"
(
   uuid varchar(40) PRIMARY KEY NOT NULL,
   created timestamp NOT NULL,
   created_by varchar(255),
   modified timestamp,
   modified_by varchar(255),
   jpaversion int NOT NULL,
   acc_exp_since timestamp,
   acc_lock_since timestamp,
   acc_non_exp bool NOT NULL,
   acc_non_lock bool NOT NULL,
   cred_exp_since timestamp,
   cred_non_exp bool NOT NULL,
   email varchar(255),
   firstname varchar(255),
   last_login timestamp,
   last_login_attempt timestamp,
   lastname varchar(255),
   locale varchar(5) NOT NULL,
   login_attempts int NOT NULL,
   password varchar(255) NOT NULL,
   password_date timestamp,
   pwd_link_created timestamp,
   pwd_link_hash varchar(255),
   phone varchar(255),
   timezone varchar(255) NOT NULL,
   username varchar(255) NOT NULL
)
;
CREATE TABLE "at_user_clients"
(
   user_uuid varchar(40) NOT NULL,
   client_uuid varchar(40) NOT NULL,
   CONSTRAINT at_user_clients_pkey PRIMARY KEY (user_uuid,client_uuid)
)
;
CREATE TABLE "at_user_group"
(
   uuid varchar(40) PRIMARY KEY NOT NULL,
   created timestamp NOT NULL,
   created_by varchar(255),
   modified timestamp,
   modified_by varchar(255),
   jpaversion int NOT NULL,
   active bool NOT NULL,
   description varchar(255),
   display_name varchar(255),
   name varchar(255) NOT NULL
)
;
CREATE TABLE "at_user_usergroups"
(
   user_uuid varchar(40) NOT NULL,
   usergroup_uuid varchar(40) NOT NULL,
   CONSTRAINT at_user_usergroups_pkey PRIMARY KEY (user_uuid,usergroup_uuid)
)
;
CREATE TABLE "at_usergroups_roles"
(
   usergroup_uuid varchar(40) NOT NULL,
   role_uuid varchar(40) NOT NULL,
   CONSTRAINT at_usergroups_roles_pkey PRIMARY KEY (usergroup_uuid,role_uuid)
)
;
CREATE UNIQUE INDEX IDX_AT_CLIENT_NAME ON "at_client"(name)
;
CREATE UNIQUE INDEX IDX_AT_CLIENT_UUID ON "at_client"(uuid)
;
CREATE UNIQUE INDEX IDX_AT_ROLE_UUID ON "at_role"(uuid)
;
CREATE UNIQUE INDEX IDX_AT_ROLE_NAME ON "at_role"(name)
;
CREATE UNIQUE INDEX IDX_AT_USER_USERNAME ON "at_user"(username)
;
CREATE UNIQUE INDEX IDX_AT_USER_UUID ON "at_user"(uuid)
;
ALTER TABLE "at_user_clients"
ADD CONSTRAINT FK_AT_USER_CLIENTS_USER
FOREIGN KEY (user_uuid)
REFERENCES "at_user"(uuid)
;
ALTER TABLE "at_user_clients"
ADD CONSTRAINT FK_AT_USER_CLIENTS_CLIENT
FOREIGN KEY (client_uuid)
REFERENCES "at_client"(uuid)
;
CREATE UNIQUE INDEX IDX_AT_USER_CLIENTS ON "at_user_clients"
(
  user_uuid,
  client_uuid
)
;
CREATE UNIQUE INDEX IDX_AT_USERGROUP_NAME ON "at_user_group"(name)
;
CREATE UNIQUE INDEX IDX_AT_USERGROUP_UUID ON "at_user_group"(uuid)
;
ALTER TABLE "at_user_usergroups"
ADD CONSTRAINT FK_AT_USER_USERGROUPS_USER
FOREIGN KEY (user_uuid)
REFERENCES "at_user"(uuid)
;
ALTER TABLE "at_user_usergroups"
ADD CONSTRAINT FK_AT_USER_USERGROUPS_UG
FOREIGN KEY (usergroup_uuid)
REFERENCES "at_user_group"(uuid)
;
CREATE UNIQUE INDEX IDX_AT_USER_USERGROUP ON "at_user_usergroups"
(
  user_uuid,
  usergroup_uuid
)
;
ALTER TABLE "at_usergroups_roles"
ADD CONSTRAINT FK_AT_USERGROUPS_ROLES_ROLE
FOREIGN KEY (role_uuid)
REFERENCES "at_role"(uuid)
;
ALTER TABLE "at_usergroups_roles"
ADD CONSTRAINT FK_AT_USERGROUPS_ROLES_UG
FOREIGN KEY (usergroup_uuid)
REFERENCES "at_user_group"(uuid)
;
CREATE UNIQUE INDEX IDX_AT_USERGROUP_ROLES ON "at_usergroups_roles"
(
  usergroup_uuid,
  role_uuid
)
;
