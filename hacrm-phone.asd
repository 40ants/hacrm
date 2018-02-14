(defsystem hacrm-phone
  :version "0.0.1"
  :author "Artemenko Alexander <svetlyak.40wt@gmail.com>"
  :maintainer "Artemenko Alexander <svetlyak.40wt@gmail.com>"
  :licence "BSD"
  :description "HACRM plugin to bind phone numbers to the contacts."
  :class :package-inferred-system
  :pathname "src/plugins/phone"
  :depends-on ((:version :asdf "3.3.1")
               "hacrm-phone/plugin"))
