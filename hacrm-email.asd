(defsystem hacrm-email
  :version "0.0.1"
  :author "Artemenko Alexander <svetlyak.40wt@gmail.com>"
  :maintainer "Artemenko Alexander <svetlyak.40wt@gmail.com>"
  :licence "BSD"
  :description "HACRM plugin to track email communications."
  :class :package-inferred-system
  :pathname "src/plugins/email"
  :depends-on ((:version :asdf "3.3.1")
               "hacrm-email/plugin"))
