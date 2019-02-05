;;; Guile IHS --- IHS command-line interface.
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (test-account)
  #:use-module (guix tests)
  #:use-module (ihs scripts)
  #:use-module (ihs scripts web)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(define test-website-json "[
  {
    \"@type\": \"WebSite\",
    \"id\": \"5ac4a2f137c47a00072b9350\",
    \"name\": \"ac-208112.ru\",
    \"accountId\": \"208112\",
    \"switchedOn\": false,
    \"unixAccount\": {
      \"@type\": \"UnixAccount\",
      \"id\": \"5ac4930337c47a00072b9341\",
      \"name\": \"u7590\",
      \"accountId\": \"208112\",
      \"switchedOn\": false,
      \"uid\": 14858,
      \"homeDir\": \"/home/u7590\",
      \"serverId\": \"web_server_134\",
      \"quota\": 10737418240,
      \"quotaUsed\": 523423744,
      \"writable\": true,
      \"sendmailAllowed\": true,
      \"passwordHash\": \"$6$DTlxPNlV$DG9wj2EyD4EntP/bv5SWTrte7pk8j2AhwdQT1QMOWCOYK6Vuo18vrlIir01nQ44gwPSdq4YVXPf6AhFferWci/\",
      \"keyPair\": {
        \"privateKey\": \"-----BEGIN RSA PRIVATE KEY-----\\nMIIEpAIBAAKCAQEAnS5wNIICj7BjEfX9KvAGqUr6jXW8O3XInsP6wgXka/AWBSfk\\nU0Hbqa16hj5IzrP/B1K0YexoEYxrupl8P6aJo/0ryFYg6RxU1yD4ifBEbMV8Hthj\\nEPKjOAokpBbpj4Ac91FwNuevJ9jRNlqRUnJ1gO/dnOp9nEocJtR9vCVLBk1zClb6\\nykeEtQVZbAuUFlmMnptldi6dlLJ54srCv7+uzKRt3t0ve/LZwaLnNycjXZFYClS6\\n77eJAn3YcAKqDVbO29oskkyMJsuoYh5x1Plx/Q3cOFEmldmW4QfY4S15tgtaYVLi\\nmJrOcHvRjRodKyQbm2zVrmpM6SkHcS4pZII6owIDAQABAoIBAFZuoNje3RWygufv\\ngsXHs4IIbvq20UlJHYbgcdWbc94+6XzwUMfPoUEO2h9TxKmVpRmbywFGBHwR4XN8\\nzywZIfsBTsAdTH+jSqv5v3Hqo1XEytoTV3aMVmCMg1WQG+MDKep959zpThH1UiY3\\na04RDrhWLmEJOQVmY1Ce7H03bKgLsB0Y8YT13UjlZDfxoQzNTxMHWMgjf5K2bYn2\\nvjCsyKY2sMUpwfYs2sQVmcx9EqCVRfvJosr3x6CKC0OWzF6U6ys9nKl0nFglfg4n\\nLwYAKYdL5LjPE9z0DPGduLH5jeL+Jti5QFwOE+9FK1Kd9oRu6pPF5p4onaenqt8D\\nz3NDTjECgYEA/Vv2ouXcMqzLWiSvv5et6BC+d7/IHMpMcvUidSeTxKIXTJUxCSak\\n+MTWnXfqVan1LyCZbaYmPO2+k7wFzMj+qGD1DLqPxdRyb6RzlNDNiEE9IQz3X/V1\\nHOCMu2U84udxTxGoPnDzLyAxTEe6lEzQe97BEAce8sS+Axt8Pv4jfz0CgYEAntHY\\nIl/T2v+j/Ue+TrdtAQbOXhTDRz/2nLn/ekB6RbmF9mz1nxv8iSp8X43ySvu9pGit\\nCKI+uHc4Qi1Cm56qK583pAFFMaM+4UiiOzAFVFzbvdl50pa7cJIWB08OsfelaT4/\\ncKT5Pi73oaTzRHx8SsXcEul2fJB/SXKziC1+P18CgYEA+nDDVba9eWIRCSQmjc7T\\nWwfiHuD2YG8gLqpdy0y55q8LNOpstEz82eNI+hMlElSURmeFAjwwpB2mOsarPQiZ\\nobtlv5VStQ0RqYX3smzMHhnO4TK9/J7Xixn0QvEJxIlfGfesRTzFRhoeYPauWhWs\\nKCXC1aMUVolgAhZTNLlmQUkCgYEAgr9cwMfFQJiOp9glWuZ0zZCN2jQbo4+L9fDF\\n2x4zyyz1lS6Ucx12JHIenAfevU5MDIrEUMRJAFHdtdVO7ZCMpYbZJqKJFVlATfUe\\n4b0h1hbwLHZabTz8KKuLiQ7TmT24JlFBpvmZ21CNOrzyLKKTLHeBOuyKR/gpYg0U\\nsfpJ4d8CgYACdO+aBOAMcrzRwFDPg5aT8x3sJs5nf2PpC1EHJb5fPQ2vnZuc3VAN\\nLDqxJdTd3GD1TyQgwP6unvazu08rNHrh5u/UGYNnlwxoC8IT+1hF3VEVgyfbw7Mv\\ngq12c61CKUbqg/XLJQMkjca1/POozgqChOtH9K3f2Cqyi0BCBxokgA==\\n-----END RSA PRIVATE KEY-----\\n\",
        \"publicKey\": \"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCdLnA0ggKPsGMR9f0q8AapSvqNdbw7dciew/rCBeRr8BYFJ+RTQduprXqGPkjOs/8HUrRh7GgRjGu6mXw/pomj/SvIViDpHFTXIPiJ8ERsxXwe2GMQ8qM4CiSkFumPgBz3UXA2568n2NE2WpFScnWA792c6n2cShwm1H28JUsGTXMKVvrKR4S1BVlsC5QWWYyem2V2Lp2UsnniysK/v67MpG3e3S978tnBouc3JyNdkVgKVLrvt4kCfdhwAqoNVs7b2iySTIwmy6hiHnHU+XH9Ddw4USaV2ZbhB9jhLXm2C1phUuKYms5we9GNGh0rJBubbNWuakzpKQdxLilkgjqj \\n\"
      },
      \"crontab\": [],
      \"infected\": false,
      \"locked\": false,
      \"willBeDeleted\": false
    },
    \"serviceId\": \"590887c9719fca7d7243e9fa\",
    \"documentRoot\": \"ac-208112.ru/www\",
    \"domains\": [
      {
        \"@type\": \"Domain\",
        \"id\": \"5ac4a28837c47a00072b934f\",
        \"name\": \"ac-208112.ru\",
        \"accountId\": \"208112\",
        \"switchedOn\": false,
        \"person\": null,
        \"sslCertificate\": null,
        \"regSpec\": null,
        \"dnsResourceRecords\": [
          {
            \"@type\": \"DNSResourceRecord\",
            \"id\": null,
            \"name\": \"ac-208112.ru\",
            \"accountId\": null,
            \"switchedOn\": true,
            \"domainId\": 10893716,
            \"recordId\": 16793323,
            \"ownerName\": \"ac-208112.ru\",
            \"ttl\": 3600,
            \"data\": \"ns.majordomo.ru. support.majordomo.ru. 2004032900 3600 900 3600000 3600\",
            \"prio\": null,
            \"rrClass\": \"IN\",
            \"rrType\": \"SOA\",
            \"locked\": false,
            \"willBeDeleted\": false
          },
          {
            \"@type\": \"DNSResourceRecord\",
            \"id\": null,
            \"name\": \"ac-208112.ru\",
            \"accountId\": null,
            \"switchedOn\": true,
            \"domainId\": 10893716,
            \"recordId\": 16793324,
            \"ownerName\": \"ac-208112.ru\",
            \"ttl\": 3600,
            \"data\": \"ns.majordomo.ru\",
            \"prio\": null,
            \"rrClass\": \"IN\",
            \"rrType\": \"NS\",
            \"locked\": false,
            \"willBeDeleted\": false
          },
          {
            \"@type\": \"DNSResourceRecord\",
            \"id\": null,
            \"name\": \"ac-208112.ru\",
            \"accountId\": null,
            \"switchedOn\": true,
            \"domainId\": 10893716,
            \"recordId\": 16793325,
            \"ownerName\": \"ac-208112.ru\",
            \"ttl\": 3600,
            \"data\": \"ns2.majordomo.ru\",
            \"prio\": null,
            \"rrClass\": \"IN\",
            \"rrType\": \"NS\",
            \"locked\": false,
            \"willBeDeleted\": false
          },
          {
            \"@type\": \"DNSResourceRecord\",
            \"id\": null,
            \"name\": \"ac-208112.ru\",
            \"accountId\": null,
            \"switchedOn\": true,
            \"domainId\": 10893716,
            \"recordId\": 16793326,
            \"ownerName\": \"ac-208112.ru\",
            \"ttl\": 3600,
            \"data\": \"ns3.majordomo.ru\",
            \"prio\": null,
            \"rrClass\": \"IN\",
            \"rrType\": \"NS\",
            \"locked\": false,
            \"willBeDeleted\": false
          },
          {
            \"@type\": \"DNSResourceRecord\",
            \"id\": null,
            \"name\": \"ac-208112.ru\",
            \"accountId\": null,
            \"switchedOn\": true,
            \"domainId\": 10893716,
            \"recordId\": 16793327,
            \"ownerName\": \"ac-208112.ru\",
            \"ttl\": 3600,
            \"data\": \"185.84.108.20\",
            \"prio\": null,
            \"rrClass\": \"IN\",
            \"rrType\": \"A\",
            \"locked\": false,
            \"willBeDeleted\": false
          },
          {
            \"@type\": \"DNSResourceRecord\",
            \"id\": null,
            \"name\": \"ac-208112.ru\",
            \"accountId\": null,
            \"switchedOn\": true,
            \"domainId\": 10893716,
            \"recordId\": 16793328,
            \"ownerName\": \"*.ac-208112.ru\",
            \"ttl\": 3600,
            \"data\": \"185.84.108.20\",
            \"prio\": null,
            \"rrClass\": \"IN\",
            \"rrType\": \"A\",
            \"locked\": false,
            \"willBeDeleted\": false
          },
          {
            \"@type\": \"DNSResourceRecord\",
            \"id\": null,
            \"name\": \"ac-208112.ru\",
            \"accountId\": null,
            \"switchedOn\": true,
            \"domainId\": 10893716,
            \"recordId\": 16793329,
            \"ownerName\": \"ac-208112.ru\",
            \"ttl\": 3600,
            \"data\": \"mmxs.majordomo.ru\",
            \"prio\": 10,
            \"rrClass\": \"IN\",
            \"rrType\": \"MX\",
            \"locked\": false,
            \"willBeDeleted\": false
          },
          {
            \"@type\": \"DNSResourceRecord\",
            \"id\": null,
            \"name\": \"ac-208112.ru\",
            \"accountId\": null,
            \"switchedOn\": true,
            \"domainId\": 10893716,
            \"recordId\": 16793330,
            \"ownerName\": \"smtp.ac-208112.ru\",
            \"ttl\": 3600,
            \"data\": \"smtp.majordomo.ru\",
            \"prio\": null,
            \"rrClass\": \"IN\",
            \"rrType\": \"CNAME\",
            \"locked\": false,
            \"willBeDeleted\": false
          },
          {
            \"@type\": \"DNSResourceRecord\",
            \"id\": null,
            \"name\": \"ac-208112.ru\",
            \"accountId\": null,
            \"switchedOn\": true,
            \"domainId\": 10893716,
            \"recordId\": 16793331,
            \"ownerName\": \"pop3.ac-208112.ru\",
            \"ttl\": 3600,
            \"data\": \"pop3.majordomo.ru\",
            \"prio\": null,
            \"rrClass\": \"IN\",
            \"rrType\": \"CNAME\",
            \"locked\": false,
            \"willBeDeleted\": false
          },
          {
            \"@type\": \"DNSResourceRecord\",
            \"id\": null,
            \"name\": \"ac-208112.ru\",
            \"accountId\": null,
            \"switchedOn\": true,
            \"domainId\": 10893716,
            \"recordId\": 16793332,
            \"ownerName\": \"mail.ac-208112.ru\",
            \"ttl\": 3600,
            \"data\": \"mail.majordomo.ru\",
            \"prio\": null,
            \"rrClass\": \"IN\",
            \"rrType\": \"CNAME\",
            \"locked\": false,
            \"willBeDeleted\": false
          }
        ],
        \"autoRenew\": false,
        \"parentDomainId\": null,
        \"synced\": [
          2018,
          4,
          4,
          14,
          4,
          11,
          20000000
        ],
        \"locked\": false,
        \"willBeDeleted\": false
      }
    ],
    \"charSet\": \"UTF8\",
    \"ssiEnabled\": false,
    \"ssiFileExtensions\": [
      \"shtml\",
      \"shtm\"
    ],
    \"cgiEnabled\": false,
    \"cgiFileExtensions\": [
      \"cgi\",
      \"pl\"
    ],
    \"scriptAlias\": \"ac-208112.ru/www/cgi-bin\",
    \"ddosProtection\": false,
    \"autoSubDomain\": false,
    \"accessByOldHttpVersion\": false,
    \"staticFileExtensions\": [
      \"ogg\",
      \"mp3\",
      \"avi\",
      \"mpeg\",
      \"js\",
      \"css\",
      \"swf\",
      \"bz2\",
      \"gz\",
      \"rar\",
      \"zip\",
      \"gif\",
      \"jpg\",
      \"png\",
      \"svg\"
    ],
    \"customUserConf\": \"\",
    \"indexFileList\": [
      \"index.php\",
      \"index.html\"
    ],
    \"accessLogEnabled\": true,
    \"errorLogEnabled\": true,
    \"followSymLinks\": true,
    \"multiViews\": false,
    \"allowUrlFopen\": true,
    \"mbstringFuncOverload\": 0,
    \"displayErrors\": null,
    \"sessionUseTransSid\": null,
    \"maxInputVars\": null,
    \"opcacheMaxAcceleratedFiles\": null,
    \"realpathCacheSize\": null,
    \"requestOrder\": null,
    \"allowUrlInclude\": null,
    \"opcacheRevalidateFreq\": null,
    \"memoryLimit\": null,
    \"mbstringInternalEncoding\": null,
    \"locked\": false,
    \"willBeDeleted\": false
  }
]")

(define test-account-json "{
  \"id\": \"208112\",
  \"version\": 100,
  \"accountId\": \"208112\",
  \"clientId\": \"208112\",
  \"planId\": \"59074671719fca0bb6bd2f95\",
  \"name\": \"AC_208112\",
  \"active\": false,
  \"created\": \"2018-04-04 11:55:29\",
  \"deactivated\": \"2018-09-09 04:42:55\",
  \"deleted\": null,
  \"accountType\": \"VIRTUAL_HOSTING\",
  \"notifications\": [
    \"EMAIL_NEWS\"
  ],
  \"settings\": {
    \"CREDIT_PERIOD\": \"P14D\",
    \"NEW_ACCOUNT\": \"false\",
    \"CREDIT\": \"false\"
  },
  \"services\": [
    {
      \"id\": \"5ad7ca67848f9e0007f252b9\",
      \"personalAccountId\": \"208112\",
      \"personalAccountName\": null,
      \"serviceId\": \"59074666719fca0bb6bd2e24\",
      \"quantity\": 1,
      \"enabled\": true,
      \"comment\": \"\",
      \"paymentService\": {
        \"id\": \"59074666719fca0bb6bd2e24\",
        \"paymentType\": \"MONTH\",
        \"cost\": 245,
        \"limit\": 1,
        \"name\": \"Безлимитный\",
        \"accountType\": \"VIRTUAL_HOSTING\",
        \"active\": true,
        \"oldId\": \"plan_9802\",
        \"servicesIdsWithLimits\": {},
        \"paymentTypeKinds\": []
      },
      \"lastBilled\": \"2018-04-24T00:00:00\",
      \"cost\": 245,
      \"name\": \"Безлимитный\"
    }
  ],
  \"ownerPersonId\": null,
  \"properties\": {
    \"angryClient\": null
  },
  \"credit\": false,
  \"creditActivationDate\": null,
  \"creditPeriod\": \"P14D\",
  \"addQuotaIfOverquoted\": false,
  \"notifyDays\": 14,
  \"autoBillSending\": false,
  \"accountNew\": false,
  \"smsPhoneNumber\": null,
  \"overquoted\": false,
  \"abonementAutoRenew\": false
}")

(define test-server-json "
[{
  \"id\": \"web_server_134\",
  \"name\": \"web33\"
}]")

(test-begin "account")

(test-assert "ihs-website"
  (mock ((ihs scripts web) account-websites
         (lambda (account)
           (match (serialize-account account)
             ("208112" test-website-json)
             (_ (error "Nonexistent account: " account)))))
        (mock ((ihs scripts web) fetch-server
               (lambda ()
                 test-server-json))
              (begin (update-cache)
                     (and (string=? (with-output-to-string
                                      (lambda ()
                                        (ihs-web "unix" "ac_208112")))
                                    "\
quota: 0.48/10.0 GB
server_id: web_server_134
server_name: web33
name: u7590
home_dir: /home/u7590

")
                          (string=? (with-output-to-string
                                      (lambda ()
                                        (ihs-web "website" "ac_208112")))
                                    "\
name: ac-208112.ru
id: 5ac4a2f137c47a00072b9350
document_root: ac-208112.ru/www
auto_sub_domain: false
index_file_list: index.html index.php
static_file_extensions: avi bz2 css gif gz jpg js mp3 mpeg ogg png rar svg swf zip
cgi_enabled: false
cgi_file_extensions: cgi pl
infected: false
ddos_protection: false

")
                          (string=? (with-output-to-string
                                      (lambda ()
                                        (ihs-web "domain" "-n" "ac_208112")))
                                    "\
name: ac-208112.ru
records: ac-208112.ru 3600 IN SOA ns.majordomo.ru. support.majordomo.ru. 2004032900 3600 900 3600000 3600
+ ac-208112.ru 3600 IN NS ns.majordomo.ru
+ ac-208112.ru 3600 IN NS ns2.majordomo.ru
+ ac-208112.ru 3600 IN NS ns3.majordomo.ru
+ ac-208112.ru 3600 IN A 185.84.108.20
+ ac-208112.ru 3600 IN A 185.84.108.20
+ ac-208112.ru 3600 IN MX mmxs.majordomo.ru
+ ac-208112.ru 3600 IN CNAME smtp.majordomo.ru
+ ac-208112.ru 3600 IN CNAME pop3.majordomo.ru
+ ac-208112.ru 3600 IN CNAME mail.majordomo.ru

"))))))

(test-assert "ihs-web"
  (mock ((ihs scripts web) fetch-account
         (lambda (account)
           (match (serialize-account account)
             ("208112" test-account-json)
             (_ (error "Nonexistent account: " account)))))
        (and (string=? (with-output-to-string
                         (lambda ()
                           (ihs-web "show" "ac_208112")))
                       "\
name: AC_208112
active: false
automatic_billing_sending: false
notify_days: true
credit: false

")
             (string=? (with-output-to-string
                         (lambda ()
                           (ihs-web "service" "ac_208112")))
                       "\
name: Безлимитный
cost: 245 rub
enabled: true
last_billed: 2018-04-24T00:00:00

"))))

(test-end "account")
