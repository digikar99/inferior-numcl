(asdf:defsystem :inferior-numcl
  :serial t
  :license "MIT"
  :version "0.1.0" ; alpha
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "A very unoptimized numcl clone that exists until numcl gets to a satisfactory level of functionality."
  :depends-on ("iterate"
               "alexandria"
               "select"
               "uiop"
               "swank"
               "select")
  :components ((:file "inferior-numcl.impl")
               (:file "inferior-numcl.exported1")
               (:file "inferior-numcl.exported2")))

