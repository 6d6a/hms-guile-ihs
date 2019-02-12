;;; Guile IHS --- IHS command-line interface.
;;; Copyright © 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (test-search)
  #:use-module (guix tests)
  #:use-module (ihs scripts)
  #:use-module (ihs scripts web)
  #:use-module (ihs scripts web search)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(define test-search-owner-json
  "{
  \"content\": [
    {
      \"id\": \"5ac493018273dc00070a67f1\",
      \"personalAccountId\": \"208112\",
      \"personalAccountName\": null,
      \"version\": 0,
      \"name\": \"Пыхалов Олег Витальевич\",
      \"type\": \"INDIVIDUAL\",
      \"contactInfo\": {
        \"phoneNumbers\": [],
        \"emailAddresses\": [
          \"go.wigust@gmail.com\"
        ],
        \"postalAddress\": null,
        \"bankName\": null,
        \"bik\": null,
        \"correspondentAccount\": null,
        \"bankAccount\": null
      },
      \"personalInfo\": null,
      \"accountId\": null
    }
  ],
  \"first\": true,
  \"last\": true,
  \"number\": 0,
  \"numberOfElements\": 1,
  \"totalElements\": 1,
  \"totalPages\": 1,
  \"size\": 20,
  \"sort\": null
}")

(define test-search-account-json
  "{
  \"content\": [
    {
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
    }
  ],
  \"first\": true,
  \"last\": true,
  \"number\": 0,
  \"numberOfElements\": 1,
  \"totalElements\": 1,
  \"totalPages\": 1,
  \"size\": 20,
  \"sort\": null
}")

(define test-search-domain-json
  "[
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
  },
  {
    \"@type\": \"Domain\",
    \"id\": \"5ace0f8cc45c1900076e3f15\",
    \"name\": \"dnl.ac-208112.ru\",
    \"accountId\": \"208112\",
    \"switchedOn\": true,
    \"person\": null,
    \"sslCertificate\": null,
    \"regSpec\": null,
    \"dnsResourceRecords\": [],
    \"autoRenew\": false,
    \"parentDomainId\": \"5ac4a28837c47a00072b934f\",
    \"synced\": [
      2018,
      4,
      11,
      18,
      4,
      17,
      304000000
    ],
    \"locked\": false,
    \"willBeDeleted\": false
  },
  {
    \"@type\": \"Domain\",
    \"id\": \"5af81d14bc72bf0007c610a6\",
    \"name\": \"foo.ac-208112.ru\",
    \"accountId\": \"208112\",
    \"switchedOn\": false,
    \"person\": null,
    \"sslCertificate\": null,
    \"regSpec\": null,
    \"dnsResourceRecords\": [
      {
        \"@type\": \"DNSResourceRecord\",
        \"id\": null,
        \"name\": \"foo.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895904,
        \"recordId\": 16883426,
        \"ownerName\": \"foo.ac-208112.ru\",
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
        \"name\": \"foo.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895904,
        \"recordId\": 16883427,
        \"ownerName\": \"foo.ac-208112.ru\",
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
        \"name\": \"foo.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895904,
        \"recordId\": 16883428,
        \"ownerName\": \"foo.ac-208112.ru\",
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
        \"name\": \"foo.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895904,
        \"recordId\": 16883429,
        \"ownerName\": \"foo.ac-208112.ru\",
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
        \"name\": \"foo.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895904,
        \"recordId\": 16883430,
        \"ownerName\": \"foo.ac-208112.ru\",
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
        \"name\": \"foo.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895904,
        \"recordId\": 16883431,
        \"ownerName\": \"*.foo.ac-208112.ru\",
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
        \"name\": \"foo.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895904,
        \"recordId\": 16883432,
        \"ownerName\": \"foo.ac-208112.ru\",
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
        \"name\": \"foo.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895904,
        \"recordId\": 16883433,
        \"ownerName\": \"smtp.foo.ac-208112.ru\",
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
        \"name\": \"foo.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895904,
        \"recordId\": 16883434,
        \"ownerName\": \"pop3.foo.ac-208112.ru\",
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
        \"name\": \"foo.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895904,
        \"recordId\": 16883435,
        \"ownerName\": \"mail.foo.ac-208112.ru\",
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
      5,
      13,
      16,
      3,
      53,
      731000000
    ],
    \"locked\": false,
    \"willBeDeleted\": false
  },
  {
    \"@type\": \"Domain\",
    \"id\": \"5af81673bc72bf0007c610a5\",
    \"name\": \"iqrealt.ac-208112.ru\",
    \"accountId\": \"208112\",
    \"switchedOn\": false,
    \"person\": null,
    \"sslCertificate\": null,
    \"regSpec\": null,
    \"dnsResourceRecords\": [
      {
        \"@type\": \"DNSResourceRecord\",
        \"id\": null,
        \"name\": \"iqrealt.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895903,
        \"recordId\": 16883348,
        \"ownerName\": \"iqrealt.ac-208112.ru\",
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
        \"name\": \"iqrealt.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895903,
        \"recordId\": 16883349,
        \"ownerName\": \"iqrealt.ac-208112.ru\",
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
        \"name\": \"iqrealt.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895903,
        \"recordId\": 16883350,
        \"ownerName\": \"iqrealt.ac-208112.ru\",
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
        \"name\": \"iqrealt.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895903,
        \"recordId\": 16883351,
        \"ownerName\": \"iqrealt.ac-208112.ru\",
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
        \"name\": \"iqrealt.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895903,
        \"recordId\": 16883352,
        \"ownerName\": \"iqrealt.ac-208112.ru\",
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
        \"name\": \"iqrealt.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895903,
        \"recordId\": 16883353,
        \"ownerName\": \"*.iqrealt.ac-208112.ru\",
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
        \"name\": \"iqrealt.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895903,
        \"recordId\": 16883354,
        \"ownerName\": \"iqrealt.ac-208112.ru\",
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
        \"name\": \"iqrealt.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895903,
        \"recordId\": 16883355,
        \"ownerName\": \"smtp.iqrealt.ac-208112.ru\",
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
        \"name\": \"iqrealt.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895903,
        \"recordId\": 16883356,
        \"ownerName\": \"pop3.iqrealt.ac-208112.ru\",
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
        \"name\": \"iqrealt.ac-208112.ru\",
        \"accountId\": null,
        \"switchedOn\": true,
        \"domainId\": 10895903,
        \"recordId\": 16883357,
        \"ownerName\": \"mail.iqrealt.ac-208112.ru\",
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
      5,
      13,
      14,
      4,
      2,
      645000000
    ],
    \"locked\": false,
    \"willBeDeleted\": false
  },
  {
    \"@type\": \"Domain\",
    \"id\": \"5ac4d8c2307559000713246a\",
    \"name\": \"joomla.ac-208112.ru\",
    \"accountId\": \"208112\",
    \"switchedOn\": true,
    \"person\": null,
    \"sslCertificate\": null,
    \"regSpec\": null,
    \"dnsResourceRecords\": [],
    \"autoRenew\": false,
    \"parentDomainId\": \"5ac4a28837c47a00072b934f\",
    \"synced\": [
      2018,
      4,
      4,
      18,
      4,
      7,
      152000000
    ],
    \"locked\": false,
    \"willBeDeleted\": false
  },
  {
    \"@type\": \"Domain\",
    \"id\": \"5b3f0a549f559a00071fd0c0\",
    \"name\": \"ng-87662-287.ac-208112.ru\",
    \"accountId\": \"208112\",
    \"switchedOn\": true,
    \"person\": null,
    \"sslCertificate\": null,
    \"regSpec\": null,
    \"dnsResourceRecords\": [],
    \"autoRenew\": false,
    \"parentDomainId\": \"5ac4a28837c47a00072b934f\",
    \"synced\": [
      2018,
      7,
      6,
      10,
      3,
      45,
      898000000
    ],
    \"locked\": false,
    \"willBeDeleted\": false
  },
  {
    \"@type\": \"Domain\",
    \"id\": \"5ac4d7333075590007132464\",
    \"name\": \"opencart.ac-208112.ru\",
    \"accountId\": \"208112\",
    \"switchedOn\": true,
    \"person\": null,
    \"sslCertificate\": null,
    \"regSpec\": null,
    \"dnsResourceRecords\": [],
    \"autoRenew\": false,
    \"parentDomainId\": \"5ac4a28837c47a00072b934f\",
    \"synced\": [
      2018,
      4,
      4,
      18,
      4,
      7,
      147000000
    ],
    \"locked\": false,
    \"willBeDeleted\": false
  },
  {
    \"@type\": \"Domain\",
    \"id\": \"5ac4a9d937c47a00072b9358\",
    \"name\": \"wordpress.ac-208112.ru\",
    \"accountId\": \"208112\",
    \"switchedOn\": true,
    \"person\": null,
    \"sslCertificate\": null,
    \"regSpec\": null,
    \"dnsResourceRecords\": [],
    \"autoRenew\": false,
    \"parentDomainId\": \"5ac4a28837c47a00072b934f\",
    \"synced\": [
      2018,
      4,
      4,
      14,
      4,
      11,
      21000000
    ],
    \"locked\": false,
    \"willBeDeleted\": false
  }
]")

(test-begin "search")

(test-assert "ihs-search-owner"
  (mock ((ihs scripts web search) search-owner
         (lambda (owner)
           (match owner
             ("go.wigust@gmail.com" test-search-owner-json)
             (_ (error "Nonexistent owner: " owner)))))
        (string=? (with-output-to-string
                        (lambda ()
                          (ihs-web-search "go.wigust@gmail.com")))
                      "\
id: 5ac493018273dc00070a67f1
account: 208112
name: Пыхалов Олег Витальевич
type: INDIVIDUAL

")))

(test-assert "ihs-search-account"
  (mock ((ihs scripts web search) search-account
         (lambda (account)
           (match (serialize-account account)
             ("208112" test-search-account-json)
             (_ (error "Nonexistent account: " account)))))
        (string=? (with-output-to-string
                    (lambda ()
                      (ihs-web-search "ac_208112")))
                      "\
created: 2018-04-04 11:55:29
id: 208112

")))

(test-assert "ihs-search-domain"
  (mock ((ihs scripts web search) search-domain
         (lambda (domain)
           (match domain
             ("ac-208112.ru" test-search-domain-json)
             (_ (error "Nonexistent domain: " domain)))))
        (string=? (with-output-to-string
                    (lambda ()
                      (ihs-web-search "ac-208112.ru")))
                  "\
name: ac-208112.ru
id: 5ac4a28837c47a00072b934f
account: 208112
active: false

name: dnl.ac-208112.ru
id: 5ace0f8cc45c1900076e3f15
account: 208112
active: true

name: foo.ac-208112.ru
id: 5af81d14bc72bf0007c610a6
account: 208112
active: false

name: iqrealt.ac-208112.ru
id: 5af81673bc72bf0007c610a5
account: 208112
active: false

name: joomla.ac-208112.ru
id: 5ac4d8c2307559000713246a
account: 208112
active: true

name: ng-87662-287.ac-208112.ru
id: 5b3f0a549f559a00071fd0c0
account: 208112
active: true

name: opencart.ac-208112.ru
id: 5ac4d7333075590007132464
account: 208112
active: true

name: wordpress.ac-208112.ru
id: 5ac4a9d937c47a00072b9358
account: 208112
active: true

")))

(test-end "search")
