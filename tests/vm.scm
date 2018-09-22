;;; Guile GMS --- GMS command-line interface.
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;;
;;; This file is part of Guile GMS.
;;;
;;; Guile GMS is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation; either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; Guile GMS is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile GMS.  If not, see <http://www.gnu.org/licenses/>.

(define-module (vm)
  #:use-module (guix tests)
  #:use-module (gms scripts)
  #:use-module (gms scripts vm)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(define test-vm-json
  "{
  \"success\": true,
  \"vds_account\": {
    \"restricted\": false,
    \"primary_ip_address\": {
      \"last_sych_date\": \"2014-06-16 16:43:54\",
      \"address\": \"78.108.95.38\",
      \"isp_license\": true,
      \"synchronize\": false,
      \"equip_ip_address_id\": \"2588\",
      \"equip_ip_network_id\": \"6\"
    },
    \"client_id\": \"4653\",
    \"vnc_port\": \"5955\",
    \"services\": [],
    \"created\": \"2016-08-17 05:47:24\",
    \"php_version\": \"php53\",
    \"vds_host_id\": \"25\",
    \"template\": {
      \"adm\": true,
      \"vds_template_id\": \"22\",
      \"vds_platform_id\": \"4\",
      \"isp_license\": true,
      \"only_for_old_plans\": false,
      \"path\": \"http://mirror.yandex.ru/debian/dists/jessie/main/installer-amd64/\",
      \"active\": true,
      \"name\": \"Debian Linux 8 KVM (ISP Manager)\",
      \"description\": \"Debian Linux 8 jessie\"
    },
    \"host\": {
      \"vnet\": false,
      \"active\": true,
      \"cpu_usage\": \"0\",
      \"vds_plan_id\": \"41\",
      \"vds_platform_id\": \"4\",
      \"equip_server_id\": \"21000\",
      \"server\": {
        \"ram_count\": \"18\",
        \"hdd_type\": \"\",
        \"server_install_date\": \"\",
        \"equip_supplier_id\": \"2\",
        \"server_remove_reason\": \"\",
        \"cpu_socket_type\": \"LGA1366\",
        \"server_remove_date\": \"\",
        \"working_days_count\": \"1678\",
        \"ram_type\": \"DDR3\",
        \"manufacturer\": \"Supermicro\",
        \"created\": \"0000-00-00 00:00:00\",
        \"ecc_ram\": true,
        \"registered_ram\": \"\",
        \"dcid\": \"\",
        \"hdd_racks\": \"\",
        \"category\": \"unknown\",
        \"cpu_count\": \"2\",
        \"unit_count\": \"2\",
        \"equip_server_type_id\": \"2\",
        \"recommended_remove_date\": \"\",
        \"ram_speed\": \"800 - 1333 ÐÐÑ\",
        \"cpu_type\": \"Intel Xeon CPU E5620 2.40GHz\",
        \"datacenter_number\": \"2593\",
        \"serial_number\": \"\",
        \"psu_power\": \"\",
        \"equip_server_id\": \"21000\",
        \"name\": \"kvm27\",
        \"cpu_socket_count\": \"2\",
        \"state\": \"unknown\",
        \"hdd_count\": \"\",
        \"colo_platform_id\": \"0\",
        \"server_warranty\": \"\",
        \"cost\": \"0.0000\",
        \"model\": \"X8DAH\",
        \"equip_rack_id\": \"7\",
        \"psu_count\": \"\"
      },
      \"vds_host_id\": \"25\",
      \"mac_address\": \"00:25:90:0B:67:C0\",
      \"cpu_capacity\": \"38400\",
      \"disk_usage\": \"1246950\",
      \"mem_capacity\": \"98304\",
      \"mem_usage\": \"40961\",
      \"disk_capacity\": \"1709036\"
    },
    \"no_auto_enable\": false,
    \"plan\": {
      \"adm\": true,
      \"active\": true,
      \"vds_plan_id\": \"41\",
      \"vds_platform_id\": \"4\",
      \"cpu_capacity\": \"2000\",
      \"name\": \"A-2\",
      \"can_add_options\": false,
      \"created\": \"2014-07-01 16:54:18\",
      \"ip_address_count\": \"1\",
      \"disk_capacity\": \"40960\",
      \"cpu_count\": \"2\",
      \"mem_capacity\": \"2048\",
      \"cost\": \"2290.0000\",
      \"cost_discount\": \"1990.0000\"
    },
    \"client_ip\": \"\",
    \"vds_account_id\": \"17803\",
    \"equip_ip_address_id\": \"2588\",
    \"vds_plan_id\": \"41\",
    \"abonement\": false,
    \"services_cost\": \"\",
    \"name\": \"vm17803\",
    \"state\": \"running\",
    \"changed\": \"2018-09-18 19:40:05\",
    \"status\": \"enabled\",
    \"vds_template_id\": \"22\",
    \"mac_address\": \"00:25:90:00:45:8B\",
    \"secret\": \"\",
    \"abonement_cost\": 23880,
    \"can_delete\": true
  }
}")

(test-begin "vm")

(test-assert "gms-vm"
  (mock ((gms scripts vm) fetch-vm
         (match-lambda
           ("vm12345" test-vm-json)
           (_ (error "Nonexistent account: " account))))
        (and (string=? (with-output-to-string
                         (lambda ()
                           (gms-vm "show" "vm12345")))
                       "\
client_id: 4653
vnc_port: 5955
state: running
status: enabled
changed: 2018-09-18 19:40:05
created: 2016-08-17 05:47:24
template_id: 22

")
             (string=? (with-output-to-string
                         (lambda ()
                           (gms-vm "ip" "vm12345")))
                       "\
ip_address: 78.108.95.38
isp_license: true

")
             (string=? (with-output-to-string
                         (lambda ()
                           (gms-vm "plan" "vm12345")))
                       "\
name: A-2
admin: true
created: 2014-07-01 16:54:18

")
             (string=? (with-output-to-string
                         (lambda ()
                           (gms-vm "server" "vm12345")))
                       "\
name: kvm27

")
             (string=? (pk (with-output-to-string
                         (lambda ()
                           (gms-vm "template" "vm12345"))))
                       "\
name: Debian Linux 8 KVM (ISP Manager)
uri: http://mirror.yandex.ru/debian/dists/jessie/main/installer-amd64/

"))))

(test-end "vm")


