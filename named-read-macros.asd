;; Copyright (c) 2017-2021, William Yao
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:

;;  * Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;;  * Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;  * Neither the name of  nor the names of its contributors may be used to
;;    endorse or promote products derived from this software without specific
;;    prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

(in-package #:asdf)
(defsystem #:named-read-macros
  :author "William Yao <williamyaoh@gmail.com>"
  :maintainer "William Yao <williamyaoh@gmail.com>"
  :description "Make read macros more Lispy. Attach read macros to symbols."
  :long-description #.(format nil "Provide a definition facility to define ~
    read macros and attach them to symbols, avoiding the possibility of ~
    collisions in readtable characters.")
  :version "1.0.0"
  :license "BSD-3"
  :depends-on ("named-readtables")
  :serial t
  :components ((:file "packages")
               (:file "named-read-macros")
               (:file "readtables"))
  :in-order-to ((test-op (test-op "named-read-macros-test"))))
