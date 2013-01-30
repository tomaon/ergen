CREATE DATABASE IF NOT EXISTS `tpce`
;

DROP USER 'tpce'@'localhost';

CREATE USER 'tpce'@'localhost' IDENTIFIED BY 'tpce';
GRANT ALL PRIVILEGES ON *.* TO 'tpce'@'localhost';

FLUSH PRIVILEGES;
