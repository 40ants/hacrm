(defsystem hacrm-birthdays
  :version "0.0.1"
  :author "Artemenko Alexander <svetlyak.40wt@gmail.com>"
  :maintainer "Artemenko Alexander <svetlyak.40wt@gmail.com>"
  :licence "BSD"
  :description "HACRM plugin to keep birthdays calendar and reminders."
  :class :package-inferred-system
  :pathname "src/plugins/birthdays"
  :depends-on ((:version :asdf "3.3.1")
               "hacrm-birthdays/plugin"))
