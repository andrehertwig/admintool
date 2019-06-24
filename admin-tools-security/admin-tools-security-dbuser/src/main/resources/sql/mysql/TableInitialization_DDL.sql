--------------------------------------------------------
--  DDL for Tables
--------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `at_client`
--

CREATE TABLE `at_client` (
  `uuid` varchar(40) NOT NULL,
  `created` datetime(6) NOT NULL,
  `created_by` varchar(50) DEFAULT NULL,
  `modified` datetime(6) DEFAULT NULL,
  `modified_by` varchar(50) DEFAULT NULL,
  `jpaversion` int(11) NOT NULL,
  `active` bit(1) NOT NULL,
  `description` varchar(255) DEFAULT NULL,
  `display_name` varchar(255) DEFAULT NULL,
  `name` varchar(50) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `at_role`
--

CREATE TABLE `at_role` (
  `uuid` varchar(40) NOT NULL,
  `created` datetime(6) NOT NULL,
  `created_by` varchar(50) DEFAULT NULL,
  `modified` datetime(6) DEFAULT NULL,
  `modified_by` varchar(50) DEFAULT NULL,
  `jpaversion` int(11) NOT NULL,
  `active` bit(1) NOT NULL,
  `description` varchar(255) DEFAULT NULL,
  `display_name` varchar(255) DEFAULT NULL,
  `name` varchar(50) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- Tabellenstruktur für Tabelle `at_user`
--

CREATE TABLE `at_user` (
  `uuid` varchar(40) NOT NULL,
  `created` datetime(6) NOT NULL,
  `created_by` varchar(50) DEFAULT NULL,
  `modified` datetime(6) DEFAULT NULL,
  `modified_by` varchar(50) DEFAULT NULL,
  `jpaversion` int(11) NOT NULL,
  `acc_exp_since` datetime(6) DEFAULT NULL,
  `acc_lock_since` datetime(6) DEFAULT NULL,
  `acc_non_exp` bit(1) NOT NULL,
  `acc_non_lock` bit(1) NOT NULL,
  `cred_exp_since` datetime(6) DEFAULT NULL,
  `cred_non_exp` bit(1) NOT NULL,
  `email` varchar(255) DEFAULT NULL,
  `firstname` varchar(255) DEFAULT NULL,
  `last_login` datetime(6) DEFAULT NULL,
  `last_login_attempt` datetime(6) DEFAULT NULL,
  `lastname` varchar(255) DEFAULT NULL,
  `locale` varchar(5) NOT NULL,
  `login_attempts` int(11) NOT NULL,
  `password` varchar(255) NOT NULL,
  `password_date` datetime(6) DEFAULT NULL,
  `pwd_link_created` datetime(6) DEFAULT NULL,
  `pwd_link_hash` varchar(255) DEFAULT NULL,
  `phone` varchar(50) DEFAULT NULL,
  `timezone` varchar(100) NOT NULL,
  `username` varchar(50) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- Tabellenstruktur für Tabelle `at_usergroups_roles`
--

CREATE TABLE `at_usergroups_roles` (
  `usergroup_uuid` varchar(40) NOT NULL,
  `role_uuid` varchar(40) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- Tabellenstruktur für Tabelle `at_user_clients`
--

CREATE TABLE `at_user_clients` (
  `user_uuid` varchar(40) NOT NULL,
  `client_uuid` varchar(40) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- Tabellenstruktur für Tabelle `at_user_group`
--

CREATE TABLE `at_user_group` (
  `uuid` varchar(40) NOT NULL,
  `created` datetime(6) NOT NULL,
  `created_by` varchar(50) DEFAULT NULL,
  `modified` datetime(6) DEFAULT NULL,
  `modified_by` varchar(50) DEFAULT NULL,
  `jpaversion` int(11) NOT NULL,
  `active` bit(1) NOT NULL,
  `description` varchar(255) DEFAULT NULL,
  `display_name` varchar(255) DEFAULT NULL,
  `name` varchar(50) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- Tabellenstruktur für Tabelle `at_user_usergroups`
--

CREATE TABLE `at_user_usergroups` (
  `user_uuid` varchar(40) NOT NULL,
  `usergroup_uuid` varchar(40) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;


--------------------------------------------------------
-- Indizes der exportierten Tabellen
--------------------------------------------------------

--
-- Indizes für die Tabelle `at_client`
--
ALTER TABLE `at_client`
  ADD PRIMARY KEY (`uuid`),
  ADD UNIQUE KEY `UK_AT_CLIENT_NAME` (`name`);

--
-- Indizes für die Tabelle `at_role`
--
ALTER TABLE `at_role`
  ADD PRIMARY KEY (`uuid`),
  ADD UNIQUE KEY `UK_AT_ROLE_NAME` (`name`);

--
-- Indizes für die Tabelle `at_user`
--
ALTER TABLE `at_user`
  ADD PRIMARY KEY (`uuid`),
  ADD UNIQUE KEY `UK_AT_USER_USERNAME` (`username`);

--
-- Indizes für die Tabelle `at_usergroups_roles`
--
ALTER TABLE `at_usergroups_roles`
  ADD PRIMARY KEY (`usergroup_uuid`,`role_uuid`),
  ADD KEY `FK_IDX_AT_USG_ROLE_ROLEID` (`role_uuid`);

--
-- Indizes für die Tabelle `at_user_clients`
--
ALTER TABLE `at_user_clients`
  ADD PRIMARY KEY (`user_uuid`,`client_uuid`),
  ADD KEY `FK_IDX_AT_USER_CLINET_CLIENTID` (`client_uuid`);

--
-- Indizes für die Tabelle `at_user_group`
--
ALTER TABLE `at_user_group`
  ADD PRIMARY KEY (`uuid`),
  ADD UNIQUE KEY `UK_AT_UG_NAME` (`name`);

--
-- Indizes für die Tabelle `at_user_usergroups`
--
ALTER TABLE `at_user_usergroups`
  ADD PRIMARY KEY (`user_uuid`,`usergroup_uuid`),
  ADD KEY `FK_IDX_AT_USG_UGID` (`usergroup_uuid`);


--------------------------------------------------------
--  Ref Constraints
--------------------------------------------------------

ALTER TABLE `at_usergroups_roles`
  ADD CONSTRAINT `FK_AT_USG_ROLES_UGID` FOREIGN KEY (`usergroup_uuid`) REFERENCES `at_user_group` (`uuid`),
  ADD CONSTRAINT `FK_AT_USG_ROLES_ROLEID` FOREIGN KEY (`role_uuid`) REFERENCES `at_role` (`uuid`);

--
-- Constraints der Tabelle `at_user_clients`
--
ALTER TABLE `at_user_clients`
  ADD CONSTRAINT `FK_AT_USER_CLIENT_USERID` FOREIGN KEY (`user_uuid`) REFERENCES `at_user` (`uuid`),
  ADD CONSTRAINT `FK_AT_USER_CLIENT_CLIENTID` FOREIGN KEY (`client_uuid`) REFERENCES `at_client` (`uuid`);

--
-- Constraints der Tabelle `at_user_usergroups`
--
ALTER TABLE `at_user_usergroups`
  ADD CONSTRAINT `FK_AT_USER_USG_UGID` FOREIGN KEY (`usergroup_uuid`) REFERENCES `at_user_group` (`uuid`),
  ADD CONSTRAINT `FK_AT_USER_USG_USERID` FOREIGN KEY (`user_uuid`) REFERENCES `at_user` (`uuid`);
COMMIT;
