;;; Guile IHS --- IHS command-line interface.
;;; Copyright © 2018, 2022 Oleg Pykhalov <go.wigust@gmail.com>
;;;
;;; This file is part of Guile IHS.
;;;
;;; Guile IHS is free software; you can redistribute it and/or modify it under
;;; the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; Guile IHS is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with Guile IHS.  If not, see <http://www.gnu.org/licenses/>.

(define-module (test-server)
  #:use-module (guix tests)
  #:use-module (ihs scripts)
  #:use-module (ihs scripts web)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(define test-server-json "
[{
  \"id\": \"web_server_134\",
  \"name\": \"web33\",
  \"switchedOn\": true,
  \"serviceIds\": [
    \"134_mysql_service\",
    \"590887bc719fca7d7243e816\",
    \"590887c8719fca7d7243e9e3\",
    \"590887c8719fca7d7243e9e4\",
    \"590887c8719fca7d7243e9e5\",
    \"590887c8719fca7d7243e9e6\",
    \"590887c9719fca7d7243e9e7\",
    \"590887c9719fca7d7243e9e8\",
    \"590887c9719fca7d7243e9e9\",
    \"590887c9719fca7d7243e9ea\",
    \"590887c9719fca7d7243e9eb\",
    \"590887c9719fca7d7243e9ec\",
    \"590887c9719fca7d7243e9ed\",
    \"590887c9719fca7d7243e9ee\",
    \"590887c9719fca7d7243e9ef\",
    \"590887c9719fca7d7243e9f0\",
    \"590887c9719fca7d7243e9f1\",
    \"590887c9719fca7d7243e9f2\",
    \"590887c9719fca7d7243e9f3\",
    \"590887c9719fca7d7243e9f4\",
    \"590887c9719fca7d7243e9f5\",
    \"590887c9719fca7d7243e9f6\",
    \"590887c9719fca7d7243e9f7\",
    \"590887c9719fca7d7243e9f8\",
    \"590887c9719fca7d7243e9f9\",
    \"590887c9719fca7d7243e9fa\",
    \"590887c9719fca7d7243e9fb\",
    \"5a2eadc02634f100080a9436\",
    \"5a2eadc02634f100080a9437\",
    \"5a2eadc02634f100080a9438\",
    \"5a2eadc02634f100080a9439\",
    \"5a2eadc02634f100080a943a\",
    \"5a2eadc12634f100080a943b\",
    \"5a2eadc12634f100080a943c\",
    \"5a2eadc12634f100080a943d\"
  ],
  \"serverRoleIds\": [
    \"59088b2f719fca053cf22297\",
    \"59088b2f719fca053cf22299\"
  ],
  \"storageIds\": [
    \"59086e28719fca5b2880566b\",
    \"59086e28719fca5b2880566c\"
  ],
  \"services\": [
    {
      \"id\": \"134_mysql_service\",
      \"name\": \"mysql@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887ba719fca7d7243e7f3\",
      \"serviceSocketIds\": [
        \"134_mysql_socket\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887ba719fca7d7243e7f3\",
        \"name\": \"mysql\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e8\",
          \"590887ba719fca7d7243e7e8\"
        ],
        \"serviceTypeName\": \"DATABASE_MYSQL\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e8\",
            \"name\": \"{config_base_path}/my.cnf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fmysql%2Fmy.cnf?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086def719fca5b28805170\",
          \"name\": \"DATABASE_MYSQL\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"134_mysql_socket\",
          \"name\": \"mysql-mysql@web33\",
          \"switchedOn\": true,
          \"port\": 3306,
          \"address\": \"185.84.108.20\"
        }
      ]
    },
    {
      \"id\": \"590887bc719fca7d7243e816\",
      \"name\": \"nginx@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887ba719fca7d7243e7f2\",
      \"serviceSocketIds\": [
        \"59086e0f719fca5b2880539c\",
        \"59086e0f719fca5b2880539d\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887ba719fca7d7243e7f2\",
        \"name\": \"nginx\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e6\",
          \"590887ba719fca7d7243e7e9\",
          \"590887ba719fca7d7243e7ea\",
          \"590887ba719fca7d7243e7eb\",
          \"5a1fe8824d51cf0007524c95\",
          \"590887ba719fca7d7243e7e6\",
          \"590887ba719fca7d7243e7e9\",
          \"590887ba719fca7d7243e7ea\",
          \"590887ba719fca7d7243e7eb\",
          \"5a1fe8824d51cf0007524c95\"
        ],
        \"serviceTypeName\": \"STAFF_NGINX\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e6\",
            \"name\": \"@NginxServer\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fnginx%2Fsites-available%2F%40NginxServer?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7e9\",
            \"name\": \"@HTTPErrorPage\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/usr%2Fshare%2Fnginx%2Fhtml%2F%40HTTPErrorPage?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ea\",
            \"name\": \"{config_base_path}/nginx.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fnginx%2Fnginx.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7eb\",
            \"name\": \"/etc/apparmor.d/usr.sbin.nginx\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.nginx?ref=master\"
          },
          {
            \"id\": \"5a1fe8824d51cf0007524c95\",
            \"name\": \"{config_base_path}/dhparam.pem\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fnginx%2Fdhparam.pem?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086def719fca5b28805171\",
          \"name\": \"STAFF_NGINX\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e0f719fca5b2880539c\",
          \"name\": \"nginx-http@web33\",
          \"switchedOn\": true,
          \"port\": 80,
          \"address\": \"185.84.108.20\"
        },
        {
          \"id\": \"59086e0f719fca5b2880539d\",
          \"name\": \"nginx-https@web33\",
          \"switchedOn\": true,
          \"port\": 443,
          \"address\": \"185.84.108.20\"
        }
      ]
    },
    {
      \"id\": \"590887c8719fca7d7243e9e3\",
      \"name\": \"apache2-php52-unsafe@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887ba719fca7d7243e7f9\",
      \"serviceSocketIds\": [
        \"59086e0f719fca5b2880539e\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887ba719fca7d7243e7f9\",
        \"name\": \"apache2-php52-unsafe\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP52_UNSAFE\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086def719fca5b28805177\",
          \"name\": \"WEBSITE_APACHE2_PHP52_UNSAFE\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e0f719fca5b2880539e\",
          \"name\": \"apache2-php52-unsafe-http@web33\",
          \"switchedOn\": true,
          \"port\": 8152,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c8719fca7d7243e9e4\",
      \"name\": \"apache2-php53-unsafe@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887ba719fca7d7243e7fd\",
      \"serviceSocketIds\": [
        \"59086e0f719fca5b2880539f\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887ba719fca7d7243e7fd\",
        \"name\": \"apache2-php53-unsafe\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP53_UNSAFE\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086def719fca5b2880517b\",
          \"name\": \"WEBSITE_APACHE2_PHP53_UNSAFE\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e0f719fca5b2880539f\",
          \"name\": \"apache2-php53-unsafe-http@web33\",
          \"switchedOn\": true,
          \"port\": 8153,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c8719fca7d7243e9e5\",
      \"name\": \"apache2-php54-unsafe@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e801\",
      \"serviceSocketIds\": [
        \"59086e0f719fca5b288053a0\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e801\",
        \"name\": \"apache2-php54-unsafe\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP54_UNSAFE\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b2880517f\",
          \"name\": \"WEBSITE_APACHE2_PHP54_UNSAFE\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e0f719fca5b288053a0\",
          \"name\": \"apache2-php54-unsafe-http@web33\",
          \"switchedOn\": true,
          \"port\": 8154,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c8719fca7d7243e9e6\",
      \"name\": \"apache2-php55-unsafe@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e805\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053a1\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e805\",
        \"name\": \"apache2-php55-unsafe\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP55_UNSAFE\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b28805183\",
          \"name\": \"WEBSITE_APACHE2_PHP55_UNSAFE\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053a1\",
          \"name\": \"apache2-php55-unsafe-http@web33\",
          \"switchedOn\": true,
          \"port\": 8155,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9e7\",
      \"name\": \"apache2-php56-unsafe@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e809\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053a2\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e809\",
        \"name\": \"apache2-php56-unsafe\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP56_UNSAFE\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b28805187\",
          \"name\": \"WEBSITE_APACHE2_PHP56_UNSAFE\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053a2\",
          \"name\": \"apache2-php56-unsafe-http@web33\",
          \"switchedOn\": true,
          \"port\": 8156,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9e8\",
      \"name\": \"apache2-php70-unsafe@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e80d\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053a3\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e80d\",
        \"name\": \"apache2-php70-unsafe\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP70_UNSAFE\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b2880518b\",
          \"name\": \"WEBSITE_APACHE2_PHP70_UNSAFE\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053a3\",
          \"name\": \"apache2-php70-unsafe-http@web33\",
          \"switchedOn\": true,
          \"port\": 8170,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9e9\",
      \"name\": \"apache2-php52-hardened_nochmod@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887ba719fca7d7243e7fa\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053a4\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887ba719fca7d7243e7fa\",
        \"name\": \"apache2-php52-hardened_nochmod\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP52_HARDENED_NOCHMOD\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086def719fca5b28805178\",
          \"name\": \"WEBSITE_APACHE2_PHP52_HARDENED_NOCHMOD\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053a4\",
          \"name\": \"apache2-php52-hardened_nochmod-http@web33\",
          \"switchedOn\": true,
          \"port\": 8252,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9ea\",
      \"name\": \"apache2-php53-hardened_nochmod@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e7fe\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053a5\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e7fe\",
        \"name\": \"apache2-php53-hardened_nochmod\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP53_HARDENED_NOCHMOD\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086def719fca5b2880517c\",
          \"name\": \"WEBSITE_APACHE2_PHP53_HARDENED_NOCHMOD\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053a5\",
          \"name\": \"apache2-php53-hardened_nochmod-http@web33\",
          \"switchedOn\": true,
          \"port\": 8253,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9eb\",
      \"name\": \"apache2-php54-hardened_nochmod@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e802\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053a6\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e802\",
        \"name\": \"apache2-php54-hardened_nochmod\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP54_HARDENED_NOCHMOD\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b28805180\",
          \"name\": \"WEBSITE_APACHE2_PHP54_HARDENED_NOCHMOD\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053a6\",
          \"name\": \"apache2-php54-hardened_nochmod-http@web33\",
          \"switchedOn\": true,
          \"port\": 8254,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9ec\",
      \"name\": \"apache2-php55-hardened_nochmod@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e806\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053a7\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e806\",
        \"name\": \"apache2-php55-hardened_nochmod\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP55_HARDENED_NOCHMOD\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b28805184\",
          \"name\": \"WEBSITE_APACHE2_PHP55_HARDENED_NOCHMOD\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053a7\",
          \"name\": \"apache2-php55-hardened_nochmod-http@web33\",
          \"switchedOn\": true,
          \"port\": 8255,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9ed\",
      \"name\": \"apache2-php56-hardened_nochmod@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e80a\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053a8\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e80a\",
        \"name\": \"apache2-php56-hardened_nochmod\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP56_HARDENED_NOCHMOD\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b28805188\",
          \"name\": \"WEBSITE_APACHE2_PHP56_HARDENED_NOCHMOD\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053a8\",
          \"name\": \"apache2-php56-hardened_nochmod-http@web33\",
          \"switchedOn\": true,
          \"port\": 8256,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9ee\",
      \"name\": \"apache2-php70-hardened_nochmod@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e80e\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053a9\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e80e\",
        \"name\": \"apache2-php70-hardened_nochmod\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP70_HARDENED_NOCHMOD\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b2880518c\",
          \"name\": \"WEBSITE_APACHE2_PHP70_HARDENED_NOCHMOD\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053a9\",
          \"name\": \"apache2-php70-hardened_nochmod-http@web33\",
          \"switchedOn\": true,
          \"port\": 8270,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9ef\",
      \"name\": \"apache2-php52-hardened@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887ba719fca7d7243e7fb\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053aa\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887ba719fca7d7243e7fb\",
        \"name\": \"apache2-php52-hardened\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP52_HARDENED\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086def719fca5b28805179\",
          \"name\": \"WEBSITE_APACHE2_PHP52_HARDENED\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053aa\",
          \"name\": \"apache2-php52-hardened-http@web33\",
          \"switchedOn\": true,
          \"port\": 8352,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9f0\",
      \"name\": \"apache2-php53-hardened@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e7ff\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053ab\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e7ff\",
        \"name\": \"apache2-php53-hardened\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP53_HARDENED\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086def719fca5b2880517d\",
          \"name\": \"WEBSITE_APACHE2_PHP53_HARDENED\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053ab\",
          \"name\": \"apache2-php53-hardened-http@web33\",
          \"switchedOn\": true,
          \"port\": 8353,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9f1\",
      \"name\": \"apache2-php54-hardened@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e803\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053ac\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e803\",
        \"name\": \"apache2-php54-hardened\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP54_HARDENED\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b28805181\",
          \"name\": \"WEBSITE_APACHE2_PHP54_HARDENED\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053ac\",
          \"name\": \"apache2-php54-hardened-http@web33\",
          \"switchedOn\": true,
          \"port\": 8354,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9f2\",
      \"name\": \"apache2-php55-hardened@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e807\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053ad\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e807\",
        \"name\": \"apache2-php55-hardened\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP55_HARDENED\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b28805185\",
          \"name\": \"WEBSITE_APACHE2_PHP55_HARDENED\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053ad\",
          \"name\": \"apache2-php55-hardened-http@web33\",
          \"switchedOn\": true,
          \"port\": 8355,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9f3\",
      \"name\": \"apache2-php56-hardened@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e80b\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053ae\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e80b\",
        \"name\": \"apache2-php56-hardened\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP56_HARDENED\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b28805189\",
          \"name\": \"WEBSITE_APACHE2_PHP56_HARDENED\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053ae\",
          \"name\": \"apache2-php56-hardened-http@web33\",
          \"switchedOn\": true,
          \"port\": 8356,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9f4\",
      \"name\": \"apache2-php70-hardened@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e80f\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053af\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e80f\",
        \"name\": \"apache2-php70-hardened\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP70_HARDENED\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b2880518d\",
          \"name\": \"WEBSITE_APACHE2_PHP70_HARDENED\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053af\",
          \"name\": \"apache2-php70-hardened-http@web33\",
          \"switchedOn\": true,
          \"port\": 8370,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9f5\",
      \"name\": \"apache2-php70-default@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e80c\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053b0\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e80c\",
        \"name\": \"apache2-php70-default\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP70_DEFAULT\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b2880518a\",
          \"name\": \"WEBSITE_APACHE2_PHP70_DEFAULT\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053b0\",
          \"name\": \"apache2-php70-default-http@web33\",
          \"switchedOn\": true,
          \"port\": 8070,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9f6\",
      \"name\": \"apache2-php52-default@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887ba719fca7d7243e7f8\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053b1\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887ba719fca7d7243e7f8\",
        \"name\": \"apache2-php52-default\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP52_DEFAULT\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086def719fca5b28805176\",
          \"name\": \"WEBSITE_APACHE2_PHP52_DEFAULT\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053b1\",
          \"name\": \"apache2-php52-default-http@web33\",
          \"switchedOn\": true,
          \"port\": 8052,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9f7\",
      \"name\": \"apache2-php53-default@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887ba719fca7d7243e7fc\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053b2\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887ba719fca7d7243e7fc\",
        \"name\": \"apache2-php53-default\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP53_DEFAULT\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086def719fca5b2880517a\",
          \"name\": \"WEBSITE_APACHE2_PHP53_DEFAULT\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053b2\",
          \"name\": \"apache2-php53-default-http@web33\",
          \"switchedOn\": true,
          \"port\": 8053,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9f8\",
      \"name\": \"apache2-php54-default@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e800\",
      \"serviceSocketIds\": [
        \"59086e10719fca5b288053b3\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e800\",
        \"name\": \"apache2-php54-default\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP54_DEFAULT\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b2880517e\",
          \"name\": \"WEBSITE_APACHE2_PHP54_DEFAULT\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e10719fca5b288053b3\",
          \"name\": \"apache2-php54-default-http@web33\",
          \"switchedOn\": true,
          \"port\": 8054,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9f9\",
      \"name\": \"apache2-php55-default@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e804\",
      \"serviceSocketIds\": [
        \"59086e11719fca5b288053b4\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e804\",
        \"name\": \"apache2-php55-default\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP55_DEFAULT\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b28805182\",
          \"name\": \"WEBSITE_APACHE2_PHP55_DEFAULT\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e11719fca5b288053b4\",
          \"name\": \"apache2-php55-default-http@web33\",
          \"switchedOn\": true,
          \"port\": 8055,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9fa\",
      \"name\": \"apache2-php56-default@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e808\",
      \"serviceSocketIds\": [
        \"59086e11719fca5b288053b5\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e808\",
        \"name\": \"apache2-php56-default\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP56_DEFAULT\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b28805186\",
          \"name\": \"WEBSITE_APACHE2_PHP56_DEFAULT\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e11719fca5b288053b5\",
          \"name\": \"apache2-php56-default-http@web33\",
          \"switchedOn\": true,
          \"port\": 8056,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"590887c9719fca7d7243e9fb\",
      \"name\": \"apache2-perl518@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"590887bb719fca7d7243e810\",
      \"serviceSocketIds\": [
        \"59086e11719fca5b288053b6\"
      ],
      \"serviceTemplate\": {
        \"id\": \"590887bb719fca7d7243e810\",
        \"name\": \"apache2-perl518\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PERL518\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"59086df0719fca5b2880518e\",
          \"name\": \"WEBSITE_APACHE2_PERL518\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"59086e11719fca5b288053b6\",
          \"name\": \"apache2-perl518-http@web33\",
          \"switchedOn\": true,
          \"port\": 7518,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"5a2eadc02634f100080a9436\",
      \"name\": \"apache2-php71-default@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"5a2a8a04ea96ca000b37e7e1\",
      \"serviceSocketIds\": [
        \"5a2e8fe8ea96ca000b37e851\"
      ],
      \"serviceTemplate\": {
        \"id\": \"5a2a8a04ea96ca000b37e7e1\",
        \"name\": \"apache2-php71-default\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP71_DEFAULT\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"5a2a8691ea96ca000b37e7d8\",
          \"name\": \"WEBSITE_APACHE2_PHP71_DEFAULT\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"5a2e8fe8ea96ca000b37e851\",
          \"name\": \"apache2-php71-default-http@web33\",
          \"switchedOn\": true,
          \"port\": 8071,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"5a2eadc02634f100080a9437\",
      \"name\": \"apache2-php72-default@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"5a2a9496ea96ca000b37e7ee\",
      \"serviceSocketIds\": [
        \"5a2e8ff9ea96ca000b37e87d\"
      ],
      \"serviceTemplate\": {
        \"id\": \"5a2a9496ea96ca000b37e7ee\",
        \"name\": \"apache2-php72-default\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP72_DEFAULT\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"5a2a8707ea96ca000b37e7db\",
          \"name\": \"WEBSITE_APACHE2_PHP72_DEFAULT\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"5a2e8ff9ea96ca000b37e87d\",
          \"name\": \"apache2-php72-default-http@web33\",
          \"switchedOn\": true,
          \"port\": 8072,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"5a2eadc02634f100080a9438\",
      \"name\": \"apache2-php71-unsafe@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"5a2a9074ea96ca000b37e7e4\",
      \"serviceSocketIds\": [
        \"5a2e8fecea96ca000b37e85c\"
      ],
      \"serviceTemplate\": {
        \"id\": \"5a2a9074ea96ca000b37e7e4\",
        \"name\": \"apache2-php71-unsafe\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP71_UNSAFE\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"5a2a86b4ea96ca000b37e7d9\",
          \"name\": \"WEBSITE_APACHE2_PHP71_UNSAFE\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"5a2e8fecea96ca000b37e85c\",
          \"name\": \"apache2-php71-unsafe-http@web33\",
          \"switchedOn\": true,
          \"port\": 8171,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"5a2eadc02634f100080a9439\",
      \"name\": \"apache2-php72-unsafe@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"5a2a955cea96ca000b37e7f2\",
      \"serviceSocketIds\": [
        \"5a2e8ffeea96ca000b37e888\"
      ],
      \"serviceTemplate\": {
        \"id\": \"5a2a955cea96ca000b37e7f2\",
        \"name\": \"apache2-php72-unsafe\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP72_UNSAFE\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"5a2a870fea96ca000b37e7dc\",
          \"name\": \"WEBSITE_APACHE2_PHP72_UNSAFE\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"5a2e8ffeea96ca000b37e888\",
          \"name\": \"apache2-php72-unsafe-http@web33\",
          \"switchedOn\": true,
          \"port\": 8172,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"5a2eadc02634f100080a943a\",
      \"name\": \"apache2-php71-hardened@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"5a2a9383ea96ca000b37e7eb\",
      \"serviceSocketIds\": [
        \"5a2e8ff1ea96ca000b37e867\"
      ],
      \"serviceTemplate\": {
        \"id\": \"5a2a9383ea96ca000b37e7eb\",
        \"name\": \"apache2-php71-hardened\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP71_HARDENED\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"5a2a8866ea96ca000b37e7de\",
          \"name\": \"WEBSITE_APACHE2_PHP71_HARDENED\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"5a2e8ff1ea96ca000b37e867\",
          \"name\": \"apache2-php71-hardened-http@web33\",
          \"switchedOn\": true,
          \"port\": 8371,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"5a2eadc12634f100080a943b\",
      \"name\": \"apache2-php72-hardened@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"5a2a960cea96ca000b37e7f8\",
      \"serviceSocketIds\": [
        \"5a2e9002ea96ca000b37e893\"
      ],
      \"serviceTemplate\": {
        \"id\": \"5a2a960cea96ca000b37e7f8\",
        \"name\": \"apache2-php72-hardened\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP72_HARDENED\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"5a2a8875ea96ca000b37e7df\",
          \"name\": \"WEBSITE_APACHE2_PHP72_HARDENED\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"5a2e9002ea96ca000b37e893\",
          \"name\": \"apache2-php72-hardened-http@web33\",
          \"switchedOn\": true,
          \"port\": 8372,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"5a2eadc12634f100080a943c\",
      \"name\": \"apache2-php71-hardened_nochmod@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"5a2a92d8ea96ca000b37e7e8\",
      \"serviceSocketIds\": [
        \"5a2e8ff5ea96ca000b37e872\"
      ],
      \"serviceTemplate\": {
        \"id\": \"5a2a92d8ea96ca000b37e7e8\",
        \"name\": \"apache2-php71-hardened_nochmod\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP71_HARDENED_NOCHMOD\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"5a2a86f5ea96ca000b37e7da\",
          \"name\": \"WEBSITE_APACHE2_PHP71_HARDENED_NOCHMOD\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"5a2e8ff5ea96ca000b37e872\",
          \"name\": \"apache2-php71-hardened_nochmod-http@web33\",
          \"switchedOn\": true,
          \"port\": 8271,
          \"address\": \"127.0.0.1\"
        }
      ]
    },
    {
      \"id\": \"5a2eadc12634f100080a943d\",
      \"name\": \"apache2-php72-hardened_nochmod@web33\",
      \"switchedOn\": true,
      \"serviceTemplateId\": \"5a2a95d3ea96ca000b37e7f5\",
      \"serviceSocketIds\": [
        \"5a2e9007ea96ca000b37e89e\"
      ],
      \"serviceTemplate\": {
        \"id\": \"5a2a95d3ea96ca000b37e7f5\",
        \"name\": \"apache2-php72-hardened_nochmod\",
        \"switchedOn\": true,
        \"configTemplateIds\": [
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\",
          \"590887ba719fca7d7243e7e7\",
          \"590887ba719fca7d7243e7ec\",
          \"590887ba719fca7d7243e7ed\",
          \"590887ba719fca7d7243e7ee\",
          \"590887ba719fca7d7243e7ef\",
          \"590887ba719fca7d7243e7f0\",
          \"5964f66687ff610007cdd09e\",
          \"59baa2b411799e000765f888\",
          \"5a9fcf53ac508f0007a44a0e\",
          \"5a9fcf7fac508f0007a44a0f\",
          \"5a9fcf9dac508f0007a44a10\"
        ],
        \"serviceTypeName\": \"WEBSITE_APACHE2_PHP72_HARDENED_NOCHMOD\",
        \"configTemplates\": [
          {
            \"id\": \"590887ba719fca7d7243e7e7\",
            \"name\": \"@ApacheVHost\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ec\",
            \"name\": \"{config_base_path}/apache2.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ed\",
            \"name\": \"{config_base_path}/modules_conf.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ee\",
            \"name\": \"{config_base_path}/modules_load.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7ef\",
            \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
          },
          {
            \"id\": \"590887ba719fca7d7243e7f0\",
            \"name\": \"/etc/init/{name}.conf\",
            \"switchedOn\": false,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
          },
          {
            \"id\": \"5964f66687ff610007cdd09e\",
            \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
          },
          {
            \"id\": \"59baa2b411799e000765f888\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf53ac508f0007a44a0e\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf7fac508f0007a44a0f\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
          },
          {
            \"id\": \"5a9fcf9dac508f0007a44a10\",
            \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
            \"switchedOn\": true,
            \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
          }
        ],
        \"serviceType\": {
          \"id\": \"5a2a8718ea96ca000b37e7dd\",
          \"name\": \"WEBSITE_APACHE2_PHP72_HARDENED_NOCHMOD\",
          \"switchedOn\": true
        }
      },
      \"serviceSockets\": [
        {
          \"id\": \"5a2e9007ea96ca000b37e89e\",
          \"name\": \"apache2-php72-hardened_nochmod-http@web33\",
          \"switchedOn\": true,
          \"port\": 8272,
          \"address\": \"127.0.0.1\"
        }
      ]
    }
  ],
  \"serverRoles\": [
    {
      \"id\": \"59088b2f719fca053cf22297\",
      \"name\": \"shared-hosting\",
      \"switchedOn\": true,
      \"serviceTemplateIds\": [
        \"590887ba719fca7d7243e7f2\",
        \"590887bb719fca7d7243e808\",
        \"590887ba719fca7d7243e7f2\",
        \"590887bb719fca7d7243e808\"
      ],
      \"serviceTemplates\": [
        {
          \"id\": \"590887ba719fca7d7243e7f2\",
          \"name\": \"nginx\",
          \"switchedOn\": true,
          \"configTemplateIds\": [
            \"590887ba719fca7d7243e7e6\",
            \"590887ba719fca7d7243e7e9\",
            \"590887ba719fca7d7243e7ea\",
            \"590887ba719fca7d7243e7eb\",
            \"5a1fe8824d51cf0007524c95\",
            \"590887ba719fca7d7243e7e6\",
            \"590887ba719fca7d7243e7e9\",
            \"590887ba719fca7d7243e7ea\",
            \"590887ba719fca7d7243e7eb\",
            \"5a1fe8824d51cf0007524c95\"
          ],
          \"serviceTypeName\": \"STAFF_NGINX\",
          \"configTemplates\": [
            {
              \"id\": \"590887ba719fca7d7243e7e6\",
              \"name\": \"@NginxServer\",
              \"switchedOn\": false,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fnginx%2Fsites-available%2F%40NginxServer?ref=master\"
            },
            {
              \"id\": \"590887ba719fca7d7243e7e9\",
              \"name\": \"@HTTPErrorPage\",
              \"switchedOn\": false,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/usr%2Fshare%2Fnginx%2Fhtml%2F%40HTTPErrorPage?ref=master\"
            },
            {
              \"id\": \"590887ba719fca7d7243e7ea\",
              \"name\": \"{config_base_path}/nginx.conf\",
              \"switchedOn\": false,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fnginx%2Fnginx.conf?ref=master\"
            },
            {
              \"id\": \"590887ba719fca7d7243e7eb\",
              \"name\": \"/etc/apparmor.d/usr.sbin.nginx\",
              \"switchedOn\": false,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.nginx?ref=master\"
            },
            {
              \"id\": \"5a1fe8824d51cf0007524c95\",
              \"name\": \"{config_base_path}/dhparam.pem\",
              \"switchedOn\": true,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fnginx%2Fdhparam.pem?ref=master\"
            }
          ],
          \"serviceType\": {
            \"id\": \"59086def719fca5b28805171\",
            \"name\": \"STAFF_NGINX\",
            \"switchedOn\": true
          }
        },
        {
          \"id\": \"590887bb719fca7d7243e808\",
          \"name\": \"apache2-php56-default\",
          \"switchedOn\": true,
          \"configTemplateIds\": [
            \"590887ba719fca7d7243e7e7\",
            \"590887ba719fca7d7243e7ec\",
            \"590887ba719fca7d7243e7ed\",
            \"590887ba719fca7d7243e7ee\",
            \"590887ba719fca7d7243e7ef\",
            \"590887ba719fca7d7243e7f0\",
            \"5964f66687ff610007cdd09e\",
            \"59baa2b411799e000765f888\",
            \"5a9fcf53ac508f0007a44a0e\",
            \"5a9fcf7fac508f0007a44a0f\",
            \"5a9fcf9dac508f0007a44a10\",
            \"590887ba719fca7d7243e7e7\",
            \"590887ba719fca7d7243e7ec\",
            \"590887ba719fca7d7243e7ed\",
            \"590887ba719fca7d7243e7ee\",
            \"590887ba719fca7d7243e7ef\",
            \"590887ba719fca7d7243e7f0\",
            \"5964f66687ff610007cdd09e\",
            \"59baa2b411799e000765f888\",
            \"5a9fcf53ac508f0007a44a0e\",
            \"5a9fcf7fac508f0007a44a0f\",
            \"5a9fcf9dac508f0007a44a10\"
          ],
          \"serviceTypeName\": \"WEBSITE_APACHE2_PHP56_DEFAULT\",
          \"configTemplates\": [
            {
              \"id\": \"590887ba719fca7d7243e7e7\",
              \"name\": \"@ApacheVHost\",
              \"switchedOn\": false,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fsites-available%2F%40ApacheVHost?ref=master\"
            },
            {
              \"id\": \"590887ba719fca7d7243e7ec\",
              \"name\": \"{config_base_path}/apache2.conf\",
              \"switchedOn\": false,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fapache2.conf?ref=master\"
            },
            {
              \"id\": \"590887ba719fca7d7243e7ed\",
              \"name\": \"{config_base_path}/modules_conf.conf\",
              \"switchedOn\": false,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_conf.conf?ref=master\"
            },
            {
              \"id\": \"590887ba719fca7d7243e7ee\",
              \"name\": \"{config_base_path}/modules_load.conf\",
              \"switchedOn\": false,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapache2%2Fmodules_load.conf?ref=master\"
            },
            {
              \"id\": \"590887ba719fca7d7243e7ef\",
              \"name\": \"/opt/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/etc/{name}/php.ini\",
              \"switchedOn\": false,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/opt%2Fphp%2Fetc%2Fapache2%2Fphp.ini?ref=master\"
            },
            {
              \"id\": \"590887ba719fca7d7243e7f0\",
              \"name\": \"/etc/init/{name}.conf\",
              \"switchedOn\": false,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Finit%2Fapache2.conf?ref=master\"
            },
            {
              \"id\": \"5964f66687ff610007cdd09e\",
              \"name\": \"/etc/apparmor.d/usr.sbin.apache2\",
              \"switchedOn\": true,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fapparmor.d%2Fusr.sbin.apache2?ref=master\"
            },
            {
              \"id\": \"59baa2b411799e000765f888\",
              \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/phpinfo.php\",
              \"switchedOn\": true,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fphpinfo.php?ref=master\"
            },
            {
              \"id\": \"5a9fcf53ac508f0007a44a0e\",
              \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache_stat.php\",
              \"switchedOn\": true,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache_stat.php?ref=master\"
            },
            {
              \"id\": \"5a9fcf7fac508f0007a44a0f\",
              \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/ocp.php\",
              \"switchedOn\": true,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Focp.php?ref=master\"
            },
            {
              \"id\": \"5a9fcf9dac508f0007a44a10\",
              \"name\": \"/var/www/html/{interpreter.name}{interpreter.version_major}{interpreter.version_minor}/opcache.php\",
              \"switchedOn\": true,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/var%2Fwww%2Fhtml%2Fphp%2Fopcache.php?ref=master\"
            }
          ],
          \"serviceType\": {
            \"id\": \"59086df0719fca5b28805186\",
            \"name\": \"WEBSITE_APACHE2_PHP56_DEFAULT\",
            \"switchedOn\": true
          }
        }
      ]
    },
    {
      \"id\": \"59088b2f719fca053cf22299\",
      \"name\": \"mysql-database-server\",
      \"switchedOn\": true,
      \"serviceTemplateIds\": [
        \"590887ba719fca7d7243e7f3\",
        \"590887ba719fca7d7243e7f3\"
      ],
      \"serviceTemplates\": [
        {
          \"id\": \"590887ba719fca7d7243e7f3\",
          \"name\": \"mysql\",
          \"switchedOn\": true,
          \"configTemplateIds\": [
            \"590887ba719fca7d7243e7e8\",
            \"590887ba719fca7d7243e7e8\"
          ],
          \"serviceTypeName\": \"DATABASE_MYSQL\",
          \"configTemplates\": [
            {
              \"id\": \"590887ba719fca7d7243e7e8\",
              \"name\": \"{config_base_path}/my.cnf\",
              \"switchedOn\": false,
              \"fileLink\": \"https://gitlab.intr/api/v4/projects/174/repository/files/etc%2Fmysql%2Fmy.cnf?ref=master\"
            }
          ],
          \"serviceType\": {
            \"id\": \"59086def719fca5b28805170\",
            \"name\": \"DATABASE_MYSQL\",
            \"switchedOn\": true
          }
        }
      ]
    }
  ],
  \"storages\": [
    {
      \"id\": \"59086e28719fca5b2880566b\",
      \"name\": \"web33\",
      \"switchedOn\": true,
      \"capacity\": 8589934592000.0,
      \"capacityUsed\": 1314914304.0,
      \"mountPoint\": \"/home\"
    },
    {
      \"id\": \"59086e28719fca5b2880566c\",
      \"name\": \"web33\",
      \"switchedOn\": true,
      \"capacity\": 8589934592000.0,
      \"capacityUsed\": 1314914304.0,
      \"mountPoint\": \"/mysql\"
    }
  ]
}]")

(test-begin "server")

(test-assert "ihs-web"
  (mock ((ihs hms) fetch-server
         (lambda ()
           test-server-json))
        (begin (update-cache)
               (and (string=? (with-output-to-string
                                (lambda ()
                                  (ihs-web "server-show" "web33")))
"\
id: web_server_134
name: web33
online: true

")
                    (string=? (with-output-to-string
                                (lambda ()
                                  (ihs-web "server-service" "web33")))
                              "\
id: 134_mysql_service
name: mysql@web33

id: 590887bc719fca7d7243e816
name: nginx@web33

id: 590887c8719fca7d7243e9e3
name: apache2-php52-unsafe@web33

id: 590887c8719fca7d7243e9e4
name: apache2-php53-unsafe@web33

id: 590887c8719fca7d7243e9e5
name: apache2-php54-unsafe@web33

id: 590887c8719fca7d7243e9e6
name: apache2-php55-unsafe@web33

id: 590887c9719fca7d7243e9e7
name: apache2-php56-unsafe@web33

id: 590887c9719fca7d7243e9e8
name: apache2-php70-unsafe@web33

id: 590887c9719fca7d7243e9e9
name: apache2-php52-hardened_nochmod@web33

id: 590887c9719fca7d7243e9ea
name: apache2-php53-hardened_nochmod@web33

id: 590887c9719fca7d7243e9eb
name: apache2-php54-hardened_nochmod@web33

id: 590887c9719fca7d7243e9ec
name: apache2-php55-hardened_nochmod@web33

id: 590887c9719fca7d7243e9ed
name: apache2-php56-hardened_nochmod@web33

id: 590887c9719fca7d7243e9ee
name: apache2-php70-hardened_nochmod@web33

id: 590887c9719fca7d7243e9ef
name: apache2-php52-hardened@web33

id: 590887c9719fca7d7243e9f0
name: apache2-php53-hardened@web33

id: 590887c9719fca7d7243e9f1
name: apache2-php54-hardened@web33

id: 590887c9719fca7d7243e9f2
name: apache2-php55-hardened@web33

id: 590887c9719fca7d7243e9f3
name: apache2-php56-hardened@web33

id: 590887c9719fca7d7243e9f4
name: apache2-php70-hardened@web33

id: 590887c9719fca7d7243e9f5
name: apache2-php70-default@web33

id: 590887c9719fca7d7243e9f6
name: apache2-php52-default@web33

id: 590887c9719fca7d7243e9f7
name: apache2-php53-default@web33

id: 590887c9719fca7d7243e9f8
name: apache2-php54-default@web33

id: 590887c9719fca7d7243e9f9
name: apache2-php55-default@web33

id: 590887c9719fca7d7243e9fa
name: apache2-php56-default@web33

id: 590887c9719fca7d7243e9fb
name: apache2-perl518@web33

id: 5a2eadc02634f100080a9436
name: apache2-php71-default@web33

id: 5a2eadc02634f100080a9437
name: apache2-php72-default@web33

id: 5a2eadc02634f100080a9438
name: apache2-php71-unsafe@web33

id: 5a2eadc02634f100080a9439
name: apache2-php72-unsafe@web33

id: 5a2eadc02634f100080a943a
name: apache2-php71-hardened@web33

id: 5a2eadc12634f100080a943b
name: apache2-php72-hardened@web33

id: 5a2eadc12634f100080a943c
name: apache2-php71-hardened_nochmod@web33

id: 5a2eadc12634f100080a943d
name: apache2-php72-hardened_nochmod@web33

")
                    (string=? (with-output-to-string
                                (lambda ()
                                  (ihs-web "server-storage" "web33")))
                              "\
id: 59086e28719fca5b2880566b
name: web33
online: true
capacity: 1.22/8000.0 GB

id: 59086e28719fca5b2880566c
name: web33
online: true
capacity: 1.22/8000.0 GB

")
                    (string=? (with-output-to-string
                                (lambda ()
                                  (ihs-web "server-socket" "web33")))
                    "\
id: 134_mysql_socket
name: mysql-mysql@web33
address: 185.84.108.20
port: 3306
online: true

id: 59086e0f719fca5b2880539c
name: nginx-http@web33
address: 185.84.108.20
port: 80
online: true

id: 59086e0f719fca5b2880539d
name: nginx-https@web33
address: 185.84.108.20
port: 443
online: true

id: 59086e0f719fca5b2880539e
name: apache2-php52-unsafe-http@web33
address: 127.0.0.1
port: 8152
online: true

id: 59086e0f719fca5b2880539f
name: apache2-php53-unsafe-http@web33
address: 127.0.0.1
port: 8153
online: true

id: 59086e0f719fca5b288053a0
name: apache2-php54-unsafe-http@web33
address: 127.0.0.1
port: 8154
online: true

id: 59086e10719fca5b288053a1
name: apache2-php55-unsafe-http@web33
address: 127.0.0.1
port: 8155
online: true

id: 59086e10719fca5b288053a2
name: apache2-php56-unsafe-http@web33
address: 127.0.0.1
port: 8156
online: true

id: 59086e10719fca5b288053a3
name: apache2-php70-unsafe-http@web33
address: 127.0.0.1
port: 8170
online: true

id: 59086e10719fca5b288053a4
name: apache2-php52-hardened_nochmod-http@web33
address: 127.0.0.1
port: 8252
online: true

id: 59086e10719fca5b288053a5
name: apache2-php53-hardened_nochmod-http@web33
address: 127.0.0.1
port: 8253
online: true

id: 59086e10719fca5b288053a6
name: apache2-php54-hardened_nochmod-http@web33
address: 127.0.0.1
port: 8254
online: true

id: 59086e10719fca5b288053a7
name: apache2-php55-hardened_nochmod-http@web33
address: 127.0.0.1
port: 8255
online: true

id: 59086e10719fca5b288053a8
name: apache2-php56-hardened_nochmod-http@web33
address: 127.0.0.1
port: 8256
online: true

id: 59086e10719fca5b288053a9
name: apache2-php70-hardened_nochmod-http@web33
address: 127.0.0.1
port: 8270
online: true

id: 59086e10719fca5b288053aa
name: apache2-php52-hardened-http@web33
address: 127.0.0.1
port: 8352
online: true

id: 59086e10719fca5b288053ab
name: apache2-php53-hardened-http@web33
address: 127.0.0.1
port: 8353
online: true

id: 59086e10719fca5b288053ac
name: apache2-php54-hardened-http@web33
address: 127.0.0.1
port: 8354
online: true

id: 59086e10719fca5b288053ad
name: apache2-php55-hardened-http@web33
address: 127.0.0.1
port: 8355
online: true

id: 59086e10719fca5b288053ae
name: apache2-php56-hardened-http@web33
address: 127.0.0.1
port: 8356
online: true

id: 59086e10719fca5b288053af
name: apache2-php70-hardened-http@web33
address: 127.0.0.1
port: 8370
online: true

id: 59086e10719fca5b288053b0
name: apache2-php70-default-http@web33
address: 127.0.0.1
port: 8070
online: true

id: 59086e10719fca5b288053b1
name: apache2-php52-default-http@web33
address: 127.0.0.1
port: 8052
online: true

id: 59086e10719fca5b288053b2
name: apache2-php53-default-http@web33
address: 127.0.0.1
port: 8053
online: true

id: 59086e10719fca5b288053b3
name: apache2-php54-default-http@web33
address: 127.0.0.1
port: 8054
online: true

id: 59086e11719fca5b288053b4
name: apache2-php55-default-http@web33
address: 127.0.0.1
port: 8055
online: true

id: 59086e11719fca5b288053b5
name: apache2-php56-default-http@web33
address: 127.0.0.1
port: 8056
online: true

id: 59086e11719fca5b288053b6
name: apache2-perl518-http@web33
address: 127.0.0.1
port: 7518
online: true

id: 5a2e8fe8ea96ca000b37e851
name: apache2-php71-default-http@web33
address: 127.0.0.1
port: 8071
online: true

id: 5a2e8ff9ea96ca000b37e87d
name: apache2-php72-default-http@web33
address: 127.0.0.1
port: 8072
online: true

id: 5a2e8fecea96ca000b37e85c
name: apache2-php71-unsafe-http@web33
address: 127.0.0.1
port: 8171
online: true

id: 5a2e8ffeea96ca000b37e888
name: apache2-php72-unsafe-http@web33
address: 127.0.0.1
port: 8172
online: true

id: 5a2e8ff1ea96ca000b37e867
name: apache2-php71-hardened-http@web33
address: 127.0.0.1
port: 8371
online: true

id: 5a2e9002ea96ca000b37e893
name: apache2-php72-hardened-http@web33
address: 127.0.0.1
port: 8372
online: true

id: 5a2e8ff5ea96ca000b37e872
name: apache2-php71-hardened_nochmod-http@web33
address: 127.0.0.1
port: 8271
online: true

id: 5a2e9007ea96ca000b37e89e
name: apache2-php72-hardened_nochmod-http@web33
address: 127.0.0.1
port: 8272
online: true

")))))

(test-end "server")
